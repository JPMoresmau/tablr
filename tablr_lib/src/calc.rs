use std::collections::{HashMap,HashSet};
use crate::types::*;
use crate::func::*;
use crate::parse::*;
use thiserror::Error;
use std::fmt;

type CellDependencies = HashMap<CellID,HashSet<CellID>>;
type FunctionLibrary = HashMap<String, Box<dyn Function>>;
type FormulaCache =  HashMap<String,Expr>;

pub struct Runtime {
    pub workbook: Workbook,
    formulas: FormulaCache,
    dependencies: CellDependencies,
    functions: FunctionLibrary,
}

pub type CellResult = (CellID,Result<CellValue,EvalError>);
pub type SetResult = Result<Vec<CellResult>,EvalError>;

pub fn setresult_ok(r: &SetResult) -> bool {
    match r {
        Err(_)=>false,
        Ok(v)=> v.iter().all(|t| t.1.is_ok()),
    }
}

pub fn setresult_impactedcells(r: &SetResult) -> (HashSet<CellID>,HashSet<CellID>) {
    match r {
        Err(_)=>(HashSet::new(),HashSet::new()),
        Ok(v)=>{
            let(ok,err):(Vec<(CellID,bool)>,Vec<(CellID,bool)>) =v.iter().map(|t| (t.0,t.1.is_ok())).partition(|t| t.1);
            (ok.iter().map(|t| t.0).collect(),err.iter().map(|t| t.0).collect())
        }
    }
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            workbook: Workbook::new(),
            formulas: HashMap::new(),
            dependencies: HashMap::new(),
            functions: built_in_functions(),
        }
    }

    pub fn set_value(&mut self, sheet_idx: usize, id: CellID, value: CellValue) -> SetResult {
        let os = self.workbook.sheets.get_mut(sheet_idx);
        match os{
            None => Err(EvalError::InvalidSheetIndex(sheet_idx)),
            Some(sheet) => Ok(set_sheet_value(&self.functions, &self.dependencies, &self.formulas, sheet, id, value, true))
        }
    }

    pub fn set_formula_str(&mut self, sheet_idx: usize, id: CellID, formula: &str) -> SetResult {
        match parse_expr(formula){
            Ok((rest,expr)) => 
                if rest.is_empty() {
                    self.set_formula(sheet_idx, id, expr)
                } else {
                    Err(EvalError::UnexpectedLeftover(rest.to_string()))
                }
            Err(err) =>  Err(EvalError::IncorrectFormula(format!("{}",err))),
        }
    }

    pub fn set_formula(&mut self, sheet_idx: usize, id: CellID, formula: Expr) -> SetResult {
        let os = self.workbook.sheets.get_mut(sheet_idx);
        match os{
            None => Err(EvalError::InvalidSheetIndex(sheet_idx)),
            Some(sheet) => {
                let t= format!("{}",formula);
                let deps = &mut self.dependencies;

                runtime_check(&self.formulas,sheet, id, &formula)?;

                self.formulas.insert(t.clone(), formula.clone());
                let e=sheet.cells.0.entry(id)
                    .or_insert(Cell{id,value:CellValue::Empty,formula:None});
                if let Some(oldt) = e.formula.take() {
                    if let Some(olde) = self.formulas.get(&oldt){
                        let mut orefs=HashSet::new();
                        olde.get_references(&mut orefs);
                        orefs.iter().for_each(|cid| {deps.get_mut(cid).map(|v| v.remove(&id));}
                            );
                    }
                }
                e.formula=Some(t);
                
                let mut refs=HashSet::new();
                formula.get_references(&mut refs);
                refs.iter().for_each(|cid| {deps.entry(*cid).or_insert_with(HashSet::new).insert(id);});

                let new_value =runtime_sheet_eval(&self.functions, sheet, &formula)?;

                Ok(set_sheet_value(&self.functions, &self.dependencies, &self.formulas, sheet, id, new_value, false))
            }
        }
    }

    pub fn eval(&self, sheet_idx: usize, expr: &Expr) -> Result<CellValue,EvalError> {
        runtime_eval(self, sheet_idx, expr)
    }

    pub fn load(&mut self, workbook: Workbook) -> Vec<SetResult> {
        self.workbook=workbook;
        self.formulas.clear();
        self.dependencies.clear();
        let mut ret = vec![];
        let deps = &mut self.dependencies;

        for sheet in self.workbook.sheets.iter() {
            let mut res= vec![];
            for cell in sheet.cells.0.values() {
                if let Some (f) = &cell.formula {
                    if !self.formulas.contains_key(f){
                        match parse_expr(f){
                            Ok((rest,expr)) => 
                                if rest.is_empty() {
                                    let mut refs=HashSet::new();
                                    expr.get_references(&mut refs);
                                    
                                    refs.iter().for_each(|cid| {deps.entry(*cid).or_insert_with(HashSet::new).insert(cell.id);});
                                    self.formulas.insert(f.clone(), expr);
                                    
                                } else {
                                    res.push((cell.id,Err(EvalError::UnexpectedLeftover(rest.to_string()))));
                                },
                            Err(err) =>  res.push((cell.id,Err(EvalError::IncorrectFormula(format!("{}",err))))),
                        }
                    }
                }
            }
            ret.push(Ok(res));
        }
        ret
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Runtime::new()
    }
}

fn set_sheet_value(functions: &FunctionLibrary, dependencies: &CellDependencies, formulas: &FormulaCache, sheet: &mut Sheet, id: CellID, value: CellValue, remove_formula: bool) -> Vec<(CellID,Result<CellValue,EvalError>)>{
    let mut ret =vec![];
    if remove_formula {
        let c=Cell{id,value:value.clone(),formula:None};
        sheet.set_cell(c);
    } else {
        sheet.set_cell_value(&id, value.clone());
    }
    
    ret.push((id,Ok(value)));

    if let Some(hs) = dependencies.get(&id){
        for c in hs.iter().flat_map(|cid| {
            if let Some(c) = sheet.cells.0.get(cid){
                if let Some(f) = &c.formula {
                    if let Some(e)=formulas.get(f){
                        let r =runtime_sheet_eval(functions, sheet, e);
                        match r {
                            Ok(cv) => return set_sheet_value(functions, dependencies, formulas, sheet, *cid, cv, false),
                            Err(ee) => return vec![(*cid,Err(ee))],
                        }
                    }
                }
            }
            vec![]
        }){
            ret.push(c);
        }
    }

    ret
}

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum Expr {
    Reference(CellID),
    Range{from:CellID,to:CellID},
    Function{name:String, args:Vec<Expr>},
    Value(CellValue),
}

impl Expr {
    pub fn get_references(&self, v: &mut HashSet<CellID>) {
        match self {
            Expr::Reference(id)=> {v.insert(*id);},
            Expr::Range{from,to} => range_ids(from,to).into_iter().for_each(|id| {v.insert(id);}),
            Expr::Function{name:_name, args}=>args.iter().for_each(|e| e.get_references(v)),
            _ => (),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Reference(id)=> write!(f, "{}",id),
            Expr::Range{from,to} => write!(f, "{}:{}",from,to),
            Expr::Function{name, args}=>{
                let argument_string=args.iter().map(|e| format!("{}",e)).collect::<Vec<String>>().join(",");
                write!(f,"{}({})",name,argument_string)
            },
            Expr::Value(val)=> write!(f, "{}",val),
        }
       
    }
}

fn runtime_check(formula_cache: &FormulaCache, sheet: &Sheet, id: CellID, expr: &Expr) -> Result<(),EvalError> {
    let mut s:HashSet<CellID> = HashSet::new();
    expr.get_references(&mut s);
    if s.contains(&id){
        return Err(EvalError::CycleDetected(CellIDVec{ids:vec![id]}));
    }
    let mut previous_ids=vec![];
    previous_ids.push(id);
    for cid in s.iter() {
        previous_ids.push(*cid);
        cycle_check(formula_cache, sheet, &cid, &mut previous_ids)?;
        previous_ids.pop();
    }
    Ok(())
}

fn cycle_check(formula_cache: &FormulaCache, sheet: &Sheet, id: &CellID, previous_ids: &mut Vec<CellID>) -> Result<(),EvalError> {
    if let Some(Some(f)) = sheet.get_cell(id).map(|c| &c.formula){
        if let Some(expr) = formula_cache.get(f){
            let mut s:HashSet<CellID> = HashSet::new();
            expr.get_references(&mut s);
            for cid in s.iter() {
                if previous_ids.contains(cid){
                    previous_ids.push(*cid);
                    return Err(EvalError::CycleDetected(CellIDVec{ids:previous_ids.clone()}));
                } else {
                    previous_ids.push(*cid);
                    cycle_check(formula_cache, sheet, cid, previous_ids)?;
                    previous_ids.pop();
                }
            }
        }
    }
    Ok(())
}

#[derive(Clone, PartialOrd, PartialEq, Debug, Error)]
pub enum EvalError {
    #[error("Function Error: {0}")]
    EvalFunctionError(#[source] FunctionError),
    #[error("Invalid sheet index: {0}")]
    InvalidSheetIndex(usize),
    #[error("Unknown function: {0}")]
    UnknownFunction(String),
    #[error("Cycle detected: {0}")]
    CycleDetected(CellIDVec),
    #[error("Unexpected left over text: {0}")]
    UnexpectedLeftover(String),
    #[error("Incorrect formula: {0}")]
    IncorrectFormula(String),
}


fn runtime_eval(runtime: &Runtime, sheet_idx: usize, expr: &Expr) -> Result<CellValue,EvalError> {
    match runtime.workbook.sheets.get(sheet_idx){
        None => Err(EvalError::InvalidSheetIndex(sheet_idx)),
        Some(sheet) => runtime_sheet_eval(&runtime.functions, sheet, expr),
    }
}

fn runtime_sheet_eval(functions: &FunctionLibrary, sheet: &Sheet, expr: &Expr) -> Result<CellValue,EvalError> {

    match expr {
            Expr::Value(v)=>Ok(v.clone()),
            Expr::Reference(id) => Ok(value(sheet,id)),
            Expr::Range{..} => Ok(CellValue::Empty),
            Expr::Function{name,args} =>apply_function(functions,sheet,name,args),
    }
   
}

fn apply_function(functions: &FunctionLibrary, sheet: &Sheet, name: &str, args: &[Expr]) -> Result<CellValue,EvalError> {
    if let Some(f)= functions.get(name) {
        let mut params = vec![];
        args.iter().try_for_each(|a| match a {
            Expr::Range{from,to} => {
                params.append(&mut range_values(sheet, from, to));
                Ok(())
            },
            _ => {
                let r=runtime_sheet_eval(functions,sheet,a);
                match r{
                    Ok(cv) => {
                        params.push(cv);
                        Ok(())
                    },
                    Err(e)=> Err(e)
                }
            },
        })?;
        f.calculate(params).map_err(EvalError::EvalFunctionError)
    } else {
        Err(EvalError::UnknownFunction(name.to_string()))
    }
    
}

fn value(sheet: &Sheet, id: &CellID) -> CellValue {
    sheet.get_cell(id).map(|c| c.value.clone()).unwrap_or(CellValue::Empty)
}

fn range_values(sheet: &Sheet, from: &CellID, to: &CellID) -> Vec<CellValue> {
    range_ids(from, to).iter().map(|id|  value(sheet,id)).filter(|v| match v{
        CellValue::Empty=>false,
        _=>true,
    }).collect()
}

fn range_ids(from: &CellID, to: &CellID) -> Vec<CellID> {
    (from.row ..= to.row).flat_map(|r| (from.col ..= to.col).map(move |c| CellID{row:r,col:c})).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;
    use rhai::{Engine, Dynamic, RegisterFn};
   

    #[test]
    fn test_range_ids() {
        test_range_id("A1","A1",vec!["A1"]);
        test_range_id("A1","A2",vec!["A1","A2"]);
        test_range_id("A1","A3",vec!["A1","A2","A3"]);
        test_range_id("A1","B1",vec!["A1","B1"]);
        test_range_id("A1","C1",vec!["A1","B1","C1"]);
        test_range_id("A1","B2",vec!["A1","B1","A2","B2"]);
        test_range_id("A1","C3",vec!["A1","B1","C1","A2","B2","C2","A3","B3","C3"]);
    }

    fn test_range_id(from: &str, to: &str, expected: Vec<&str>) {
        assert_eq!(expected.iter().map(|s| CellID::from_str(s).unwrap()).collect::<Vec<CellID>>(),range_ids(&CellID::from_str(from).unwrap(), &CellID::from_str(to).unwrap()));
    }

    #[test]
    fn test_ref_expr() {
        let mut r=Runtime::new();

        let id = CellID::from_str("A1").unwrap();
        let c = Cell{id,value:CellValue::Integer(1),formula:None};
        

        r.workbook.sheets[0].set_cell( c);
        assert_eq!(CellValue::Integer(1),value(&r.workbook.sheets[0], &id));
        assert_eq!(Ok(CellValue::Integer(1)),r.eval(0, &Expr::Reference(id)));
    }

    #[test]
    fn test_range_expr() {
        let mut r=Runtime::new();

        let id1 = r.workbook.sheets[0].set_cell( Cell::new("A1",CellValue::Integer(1)));

        let id2 = r.workbook.sheets[0].set_cell( Cell::new("B1", CellValue::Integer(2)));

        assert_eq!(vec![CellValue::Integer(1),CellValue::Integer(2)],range_values(&r.workbook.sheets[0], &id1, &id2));
        assert_eq!(vec![CellValue::Integer(1),CellValue::Integer(2)],range_values(&r.workbook.sheets[0], &id1, &CellID::from_str("C1").unwrap()));
        assert_eq!(Ok(CellValue::Empty),r.eval(0, &Expr::Range{from:id1,to:id2}));
    }

    #[test]
    fn test_function_expr(){
        let mut r=Runtime::new();

        let id1 = r.workbook.sheets[0].set_cell(Cell::new("A1", CellValue::Integer(1)));

        let id2 = r.workbook.sheets[0].set_cell( Cell::new("B1", CellValue::Integer(2)));

        assert_eq!(Ok(CellValue::Integer(3)),r.eval(0, &Expr::Function{name:"SUM".to_string(),args:vec![Expr::Reference(id1),Expr::Reference(id2)]}));
        assert_eq!(Ok(CellValue::Integer(3)),r.eval(0, &Expr::Function{name:"SUM".to_string(),args:vec![Expr::Range{from:id1,to:id2}]}));
    }

    #[test]
    fn test_expr_change() {
        let r=&mut Runtime::new();
        
        let id1 = CellID::from_str("A1").unwrap();
        let ret1=r.set_value(0, id1, CellValue::Integer(1));
        assert_eq!(Ok(vec![(id1,Ok(CellValue::Integer(1)))]),ret1);
        
        let id2 = CellID::from_str("A2").unwrap();
        let ret2= r.set_value(0, id2, CellValue::Integer(1));
        assert_eq!(Ok(vec![(id2,Ok(CellValue::Integer(1)))]),ret2);

        let id3 = CellID::from_str("A3").unwrap();
        let ret3 = r.set_formula_str(0, id3, "SUM(A1,A2)");
        assert_eq!(Ok(vec![(id3,Ok(CellValue::Integer(2)))]),ret3);

        let ret1=r.set_value(0, id1, CellValue::Integer(2));
        assert_eq!(Ok(vec![(id1,Ok(CellValue::Integer(2))),(id3,Ok(CellValue::Integer(3)))]),ret1);
    }

    #[test]
    fn test_dependencies_load() -> Result<(),EvalError> {
        let r=&mut Runtime::new();
        
        let id1 = CellID::from_str("A1").unwrap();
        r.set_value(0, id1, CellValue::Integer(1))?;
       
        let id2 = CellID::from_str("A2").unwrap();
        r.set_value(0, id2, CellValue::Integer(1))?;
       
        let id3 = CellID::from_str("A3").unwrap();
        r.set_formula_str(0, id3, "SUM(A1,A2)")?;
        
        assert_eq!(1,r.formulas.len());
        assert!(r.formulas.contains_key("SUM(A1,A2)"));
        assert_eq!(Some(&[id3].iter().cloned().collect()),r.dependencies.get(&id1));
        assert_eq!(Some(&[id3].iter().cloned().collect()),r.dependencies.get(&id2));

        let mut r2=Runtime::new();
        r2.load(r.workbook.clone());
        assert_eq!(1,r2.formulas.len());
        assert!(r2.formulas.contains_key("SUM(A1,A2)"));
        assert_eq!(Some(&[id3].iter().cloned().collect()),r2.dependencies.get(&id1));
        assert_eq!(Some(&[id3].iter().cloned().collect()),r2.dependencies.get(&id2));

        Ok(())
    }


    #[test]
    fn test_cycles(){
        let r=&mut Runtime::new();
        
        let id1 = CellID::from_str("A1").unwrap();
        let ret1 = r.set_formula(0, id1, Expr::Reference(id1));
        assert_eq!(Err(EvalError::CycleDetected(CellIDVec{ids:vec![id1]})), ret1);

        let id2 = CellID::from_str("A2").unwrap();

        let ret1 = r.set_formula(0, id1, Expr::Reference(id2));
        assert!(ret1.is_ok());

        let ret2 = r.set_formula(0, id2, Expr::Reference(id1));
        assert_eq!(Err(EvalError::CycleDetected(CellIDVec{ids:vec![id2,id1,id2]})), ret2);

    }

    #[test]
    fn test_rhai(){
        let mut engine = Engine::new();

        let result = engine.eval_expression::<i64>("40 + 2");
        assert!(matches!(result,Ok(x) if x==42));

        engine.on_var(|name,_,_| {
            match name {
                "A1"=> Ok(Some(Dynamic::from(CellValue::Integer(40)))),
                "A2"=> Ok(Some(Dynamic::from(CellValue::Integer(2)))),
                _ => Ok(None)
            }
        });
        engine.register_type::<CellValue>();
        println!("{}",CellValue::Integer(40) + CellValue::Integer(2));
        engine.register_fn("+", add_v);

        let rast= engine.compile_expression("A1 + A2");
        if let Ok(ast) = rast {
            println!("vars: {:?}",ast.extract_variables());
            let result= engine.eval_ast::<CellValue>(&ast);
            println!("{:?}",result);
            assert!(matches!(result,Ok(x) if x==CellValue::Integer(42)));
        }
        engine.on_var(|name,_,_| {
            match name {
                "R1"=> Ok(Some(Dynamic::from(vec![10_f64,20_f64,30_f64,40_f64]))),
                _ => Ok(None)
            }
        });

        engine.register_fn("avg",avg);
        engine.register_fn("avg",avg_f);

        let result = engine.eval_expression::<f64>("avg(R1)");
        println!("avg:{:?}",result);
        assert!(matches!(result,Ok(x) if x==25.0));

    }

    fn avg(vs: Vec<i64>) -> f64 {
        vs.iter().sum::<i64>() as f64 / vs.len() as f64
    }

    fn avg_f(vs: Vec<f64>) -> f64 {
        vs.iter().sum::<f64>() / vs.len() as f64
    }

    fn add_v(cv1: CellValue, cv2: CellValue) -> CellValue{
        return cv1+cv2;
    }

    
}