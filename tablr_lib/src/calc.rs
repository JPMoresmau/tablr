use std::collections::{HashMap,HashSet};
use crate::types::*;
use crate::func::*;
use thiserror::Error;
use std::fmt;

use rhai::{Engine, Dynamic, RegisterFn, EvalAltResult, Scope};
use rhai::{AST,Stmt};

type CellDependencies = HashMap<CellID,HashSet<CellID>>;
type FunctionLibrary = HashMap<String, Box<dyn Function>>;
type FormulaCache =  HashMap<String,AST>;

pub struct Runtime {
    pub workbook: Workbook,
    engine: Engine,
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

fn build_engine() -> Engine{
    let mut engine = Engine::new();

   
    /*engine.on_var(|name,_,_| {
        match name {
            "A1"=> Ok(Some(Dynamic::from(CellValue::Integer(40)))),
            "A2"=> Ok(Some(Dynamic::from(CellValue::Integer(2)))),
            _ => Ok(None)
        }
    });*/
    engine.register_type::<CellValue>();
    engine.register_fn("+", |cv1:CellValue,cv2:CellValue| cv1+cv2);
    engine
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            workbook: Workbook::new(),
            engine: build_engine(),
            formulas: HashMap::new(),
            dependencies: HashMap::new(),
            functions: built_in_functions(),
        }
    }

    pub fn set_value(&mut self, sheet_idx: usize, id: CellID, value: CellValue) -> SetResult {
        let os = self.workbook.sheets.get_mut(sheet_idx);
        match os{
            None => Err(EvalError::InvalidSheetIndex(sheet_idx)),
            Some(sheet) => Ok(set_sheet_value(&mut self.engine, &self.dependencies, &self.formulas, sheet, id, value, true))
        }
    }

    pub fn set_formula_str(&mut self, sheet_idx: usize, id: CellID, formula: &str) -> SetResult {
        match self.engine.compile(formula){
            Ok(expr) => self.set_formula(sheet_idx, id, formula, expr),
            Err(err) =>  Err(EvalError::IncorrectFormula(format!("{}",err))),
        }
    }

    fn set_formula(&mut self, sheet_idx: usize, id: CellID, text: &str, formula:AST) -> SetResult {
        let os = self.workbook.sheets.get_mut(sheet_idx);
        match os{
            None => Err(EvalError::InvalidSheetIndex(sheet_idx)),
            Some(sheet) => {
               
                let deps = &mut self.dependencies;

                runtime_check(&self.formulas,sheet, id, &formula)?;

                self.formulas.insert(text.to_string(), formula.clone());
                let e=sheet.cells.0.entry(id)
                    .or_insert(Cell{id,value:CellValue::Empty,formula:None});
                if let Some(oldt) = e.formula.take() {
                    if let Some(olde) = self.formulas.get(&oldt){
                        let orefs= get_references(olde);
                        orefs.iter().for_each(|cid| {deps.get_mut(cid).map(|v| v.remove(&id));}
                            );
                    }
                }
                e.formula=Some(text.to_string());
                
                let refs=get_references(&formula);
                refs.iter().for_each(|cid| {deps.entry(*cid).or_insert_with(HashSet::new).insert(id);});

                let new_value =runtime_sheet_eval(&mut self.engine, sheet, &formula)?;

                Ok(set_sheet_value(&mut self.engine, &self.dependencies, &self.formulas, sheet, id, new_value, false))
            }
        }
    }

    pub fn eval(&mut self, sheet_idx: usize, expr: &AST) -> Result<CellValue,EvalError> {
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
                        match self.engine.compile(f){
                            Ok(expr) => {
                                    let refs=get_references(&expr);
                                    refs.iter().for_each(|cid| {deps.entry(*cid).or_insert_with(HashSet::new).insert(cell.id);});
                                    self.formulas.insert(f.clone(), expr);
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

fn set_sheet_value(engine:&mut Engine, dependencies: &CellDependencies, formulas: &FormulaCache, sheet: &mut Sheet, id: CellID, value: CellValue, remove_formula: bool) -> Vec<(CellID,Result<CellValue,EvalError>)>{
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
                        let r =runtime_sheet_eval(engine, sheet, e);
                        match r {
                            Ok(cv) => return set_sheet_value(engine, dependencies, formulas, sheet, *cid, cv, false),
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

/*
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
}*/

fn runtime_check(formula_cache: &FormulaCache, sheet: &Sheet, id: CellID, expr: &AST) -> Result<(),EvalError> {
    let s=get_references(expr);
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
            let s = get_references(expr);
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


#[derive(Debug, Error, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum EvalError {
    #[error("Function Error: {0}")]
    EvalFunctionError(String),
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


fn runtime_eval(runtime: &mut Runtime, sheet_idx: usize, expr: &AST) -> Result<CellValue,EvalError> {
    match runtime.workbook.sheets.get(sheet_idx){
        None => Err(EvalError::InvalidSheetIndex(sheet_idx)),
        Some(sheet) => runtime_sheet_eval(&mut runtime.engine, sheet, expr),
    }
}

fn runtime_sheet_eval(engine: &mut Engine, sheet: &Sheet, expr: &AST) -> Result<CellValue,EvalError> {
    let mut scope = Scope::new();
    scope.push("sheet", sheet.clone());
    engine.on_var(|name,_,context| {
        let sheet2 = context.scope.get_value::<Sheet>("sheet").unwrap();
        if let Ok(r) = name.parse::<CellRange>() {
            return Ok(Some(Dynamic::from(range_values(&sheet2, &r))));
        }
        if let Ok(c) = name.parse::<CellID>() {
            return Ok(Some(Dynamic::from(value(&sheet2,&c))));
        }
        Ok(None)
    });
    let r=engine.eval_ast_with_scope::<CellValue>(&mut scope, expr);
    r.map_err(|e| EvalError::EvalFunctionError(format!("{}",e)))
    /*match expr {
            Expr::Value(v)=>Ok(v.clone()),
            Expr::Reference(id) => Ok(value(sheet,id)),
            Expr::Range{..} => Ok(CellValue::Empty),
            Expr::Function{name,args} =>apply_function(functions,sheet,name,args),
    }*/
   
}
/*
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
*/

fn value(sheet: &Sheet, id: &CellID) -> CellValue {
    sheet.get_cell(id).map(|c| c.value.clone()).unwrap_or(CellValue::Empty)
}

fn range_values(sheet: &Sheet, range: &CellRange) -> Vec<CellValue> {
    range.cell_ids().iter().map(|id|  value(sheet,id)).filter(|v| match v{
        CellValue::Empty=>false,
        _=>true,
    }).collect()
}

fn get_references(ast: &AST)->HashSet<CellID> {
    extract_variables(ast)
        .iter().flat_map(|v| parse_cell_ids(v)).collect()
}

fn parse_cell_ids(s: &str) -> Vec<CellID> {
    if let Ok(r) = s.parse::<CellRange>() {
        return r.cell_ids();
    }
    if let Ok(c) = s.parse::<CellID>() {
        return vec![c];
    }
    Vec::new()
}

fn extract_variables(ast: &AST) -> HashSet<String> {
    let mut vars = HashSet::new();
    let mut defs = HashSet::new();
    for stmt in ast.statements().iter() {
        extract_stmt_variables(stmt, &mut defs, &mut vars);
    }
    defs.iter().for_each(|n| {
        vars.remove(n);
    });
    vars
}

    /// Extract variables from a statement, removing variables defined in the script itself
fn extract_stmt_variables(stmt: &Stmt, defs: &mut HashSet<String>, vars: &mut HashSet<String>) {
    match stmt {
        Stmt::IfThenElse(e, bs, _) => {
            extract_expr_variables(e, defs, vars);
            extract_stmt_variables(&bs.0, defs, vars);
            if let Some(s) = &bs.1 {
                extract_stmt_variables(s, defs, vars);
            }
        }
        Stmt::While(e, s, _) => {
            extract_expr_variables(e, defs, vars);
            extract_stmt_variables(s, defs, vars);
        }
        Stmt::Loop(s, _) => extract_stmt_variables(s, defs, vars),
        Stmt::For(e, bs, _) => {
            extract_expr_variables(e, defs, vars);
            extract_stmt_variables(&bs.1, defs, vars);
        }
        Stmt::Let(id, oe, _) => {
            if let Some(e) = &oe {
                extract_expr_variables(e, defs, vars);
            }
            defs.insert(id.name.clone());
        }
        Stmt::Const(id, oe, _) => {
            if let Some(e) = &oe {
                extract_expr_variables(e, defs, vars);
            }
            defs.insert(id.name.clone());
        }
        Stmt::Assignment(be, _) => {
            extract_expr_variables(&be.0, defs, vars);
            extract_expr_variables(&be.2, defs, vars);
        }
        Stmt::Block(ss, _) => ss
            .iter()
            .for_each(|s| extract_stmt_variables(s, defs, vars)),
        Stmt::TryCatch(bs, _, _) => {
            extract_stmt_variables(&bs.0, defs, vars);
            extract_stmt_variables(&bs.2, defs, vars);
        }
        Stmt::Expr(e) => extract_expr_variables(e, defs, vars),
        _ => (),
    };
}


/// Extract variables from an expression
fn extract_expr_variables(expr: &rhai::Expr, defs: &mut HashSet<String>, vars: &mut HashSet<String>) {
    match expr {
        rhai::Expr::Variable(x) => {
            vars.insert(x.3.name.clone());
        }
        rhai::Expr::Stmt(ss, _) => ss
            .iter()
            .for_each(|s| extract_stmt_variables(s, defs, vars)),
        rhai::Expr::Expr(e) => extract_expr_variables(e, defs, vars),
        rhai::Expr::FnCall(ci, _) => ci
            .args
            .iter()
            .for_each(|e| extract_expr_variables(e, defs, vars)),
        rhai::Expr::Dot(be, _) => {
            extract_expr_variables(&be.lhs, defs, vars);
            extract_expr_variables(&be.rhs, defs, vars);
        }
        rhai::Expr::Index(be, _) => {
            extract_expr_variables(&be.lhs, defs, vars);
            extract_expr_variables(&be.rhs, defs, vars);
        }
        rhai::Expr::Array(es, _) => es
            .iter()
            .for_each(|e| extract_expr_variables(e, defs, vars)),
        rhai::Expr::Map(es, _) => es
            .iter()
            .for_each(|(_, e)| extract_expr_variables(e, defs, vars)),
        rhai::Expr::In(be, _) => extract_expr_variables(&be.rhs, defs, vars),
        rhai::Expr::And(be, _) => {
            extract_expr_variables(&be.lhs, defs, vars);
            extract_expr_variables(&be.rhs, defs, vars);
        }
        rhai::Expr::Or(be, _) => {
            extract_expr_variables(&be.lhs, defs, vars);
            extract_expr_variables(&be.rhs, defs, vars);
        }
        _ => (),
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

   

    #[test]
    fn test_ref_expr() {
        let mut r=Runtime::new();

        let id = CellID::from_str("A1").unwrap();
        let c = Cell{id,value:CellValue::Integer(1),formula:None};
        

        r.workbook.sheets[0].set_cell( c);
        assert_eq!(CellValue::Integer(1),value(&r.workbook.sheets[0], &id));
        assert_eq!(Ok(CellValue::Integer(1)),r.eval(0, &r.engine.compile("A1").unwrap()));
    }

    #[test]
    fn test_range_expr() {
        let mut r=Runtime::new();

        let id1 = r.workbook.sheets[0].set_cell( Cell::new("A1",CellValue::Integer(1)));

        let id2 = r.workbook.sheets[0].set_cell( Cell::new("B1", CellValue::Integer(2)));
        let r1=CellRange{from:id1,to:id2};
        let r2=CellRange{from:id1,to:CellID::from_str("C1").unwrap()};
        assert_eq!(vec![CellValue::Integer(1),CellValue::Integer(2)],range_values(&r.workbook.sheets[0], &r1));
        assert_eq!(vec![CellValue::Integer(1),CellValue::Integer(2)],range_values(&r.workbook.sheets[0], &r2));
        assert_eq!(Ok(CellValue::Empty),r.eval(0, &r.engine.compile("A1_B1").unwrap()));
    }

    #[test]
    fn test_function_expr(){
        let mut r=Runtime::new();

        let id1 = r.workbook.sheets[0].set_cell(Cell::new("A1", CellValue::Integer(1)));

        let id2 = r.workbook.sheets[0].set_cell( Cell::new("B1", CellValue::Integer(2)));

        assert_eq!(Ok(CellValue::Integer(3)),r.eval(0, &r.engine.compile("A1+B1").unwrap()));
        assert_eq!(Ok(CellValue::Integer(3)),r.eval(0, &r.engine.compile("sum(A1_B1)").unwrap()));
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
        let ret3 = r.set_formula_str(0, id3, "A1+A2");
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
        let ret1 = r.set_formula_str(0, id1, "A1");
        assert_eq!(Err(EvalError::CycleDetected(CellIDVec{ids:vec![id1]})), ret1);

        let id2 = CellID::from_str("A2").unwrap();

        let ret1 = r.set_formula_str(0, id1, "A2");
        assert!(ret1.is_ok());

        let ret2 = r.set_formula_str(0, id2, "A1");
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
        engine.register_fn("+", |cv1:CellValue,cv2:CellValue| cv1+cv2);

        let rast= engine.compile_expression("A1 + A2");
        if let Ok(ast) = rast {
            println!("vars: {:?}",extract_variables(&ast));
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


    

}