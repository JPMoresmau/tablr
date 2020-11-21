use std::collections::{HashMap,HashSet};
use crate::types::*;
use crate::func::*;
use crate::parse::*;
use thiserror::Error;
use std::fmt;
use std::ops::Try;


type CellDependencies = HashMap<GlobalCellID,HashSet<GlobalCellID>>;
type FunctionLibrary = HashMap<String, Box<dyn Function>>;
type FormulaCache =  HashMap<String,Expr>;

pub struct Runtime {
    pub workbook: Workbook,
    formulas: FormulaCache,
    dependencies: CellDependencies,
    pub functions: FunctionLibrary,
}

pub type CellResult = (CellID,Result<CellValue,EvalError>);
pub type ImpactedCells = (Vec<(CellID,bool)>,Vec<(CellID,bool)>);

#[derive(PartialOrd, PartialEq, Debug, Clone)]
pub struct SetResult(pub Result<Vec<CellResult>,EvalError>);

impl SetResult {

    pub fn is_ok(&self) -> bool {
        match &self.0 {
            Err(_)=>false,
            Ok(v)=> v.iter().all(|t| t.1.is_ok()),
        }
    }


    pub fn impacted_cells(&self) -> (HashSet<CellID>,HashSet<CellID>) {
        match &self.0 {
            Err(_)=>(HashSet::new(),HashSet::new()),
            Ok(v)=>{
                let(ok,err):ImpactedCells =v.iter().map(|t| (t.0,t.1.is_ok())).partition(|t| t.1);
                (ok.iter().map(|t| t.0).collect(),err.iter().map(|t| t.0).collect())
            }
        }
    }
}

impl Try for SetResult {
    type Ok=Vec<CellResult>;
    type Error = EvalError;

    fn into_result(self) -> Result<Self::Ok, Self::Error> {
        self.0
    }

    fn from_error(e:EvalError)-> Self {
        Self(Err(e))
    }

    fn from_ok(v:Vec<CellResult>)-> Self {
        Self(Ok(v))
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
            None => SetResult::from_error(EvalError::InvalidSheetIndex(sheet_idx)),
            Some(sheet) => SetResult::from_ok(set_sheet_value(&self.functions, &self.dependencies, &self.formulas, sheet,GlobalCellID::new(sheet_idx, id), value, true))
        }
    }

    pub fn set_formula_str(&mut self, sheet_idx: usize, id: CellID, formula: &str) -> SetResult {
        match parse_cached_expr(&mut self.formulas, formula){
            Ok(expr) => self.set_formula_with_text(sheet_idx, id, formula.to_string(), expr),
            Err(err) =>  SetResult::from_error(err),
        }
    }

    

    pub fn set_formula(&mut self, sheet_idx: usize, id: CellID, formula: Expr) -> SetResult {
        let t = format!("{}",formula);
        self.formulas.entry(t).or_insert_with(|| formula.clone());
        self.set_formula_with_text(sheet_idx, id,  format!("{}",formula), formula)
    }

    pub fn set_formula_with_text(&mut self, sheet_idx: usize, id: CellID, text: String, formula: Expr) -> SetResult {
        let os = self.workbook.sheets.get_mut(sheet_idx);
        match os{
            None => SetResult::from_error(EvalError::InvalidSheetIndex(sheet_idx)),
            Some(sheet) => {
                set_formula_with_text(&self.functions, &mut self.dependencies, &self.formulas,sheet, GlobalCellID::new(sheet_idx,id), text, formula)
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

        for (sheet_idx,sheet) in self.workbook.sheets.iter().enumerate() {
            let mut res= vec![];
            for (id,f) in sheet.cells.extras.iter() {
                if !self.formulas.contains_key(f){
                    match parse_expr(f){
                        Ok((rest,expr)) => 
                            if rest.is_empty() {
                                let mut refs=HashSet::new();
                                expr.get_references(&mut refs);
                                
                                refs.iter().for_each(|cid| {deps.entry(GlobalCellID::new(sheet_idx,*cid)).or_insert_with(HashSet::new).insert(GlobalCellID::new(sheet_idx,*id));});
                                self.formulas.insert(f.clone(), expr);
                                
                            } else {
                                res.push((id.clone(),Err(EvalError::UnexpectedLeftover(rest.to_string()))));
                            },
                        Err(err) =>  res.push((id.clone(),Err(EvalError::IncorrectFormula(format!("{}",err))))),
                    }
                }
                
            }
            ret.push(SetResult::from_ok(res));
        }
        ret
    }

    pub fn copy_cell(&mut self, sheet_idx: usize, from: CellID, to: CellID) -> SetResult {
        let os = self.workbook.sheets.get_mut(sheet_idx);
        match os{
            None => SetResult::from_error(EvalError::InvalidSheetIndex(sheet_idx)),
            Some(sheet) => {
                let gto=GlobalCellID::new(sheet_idx,to);
                if let Some(s) = sheet.cells.get_cell_extra(&from).map(|c|c.clone()){
                    match parse_cached_expr(&mut self.formulas, &s){
                        Ok(expr) => set_formula_with_text(&self.functions, &mut self.dependencies, &self.formulas,sheet, gto, s, expr.copy(from,to)),
                        Err(err) =>  SetResult::from_error(err),
                    }
                } else {
                    if let Some(v) = sheet.cells.get_cell_value(&from).map(|c|c.clone()){
                        SetResult::from_ok(set_sheet_value(&self.functions, &self.dependencies, &self.formulas, sheet, gto, v, true))
                    } else {
                        SetResult::from_ok(set_sheet_value(&self.functions, &self.dependencies, &self.formulas, sheet, gto, CellValue::Empty, true))
                    }
                }
                /*let ocell= sheet.cells.get_cell(&from).map(|c| (c.value.clone(),c.dynamic.clone()));
                match ocell {
                    None=> SetResult::from_ok(set_sheet_value(&self.functions, &self.dependencies, &self.formulas, sheet, to, CellValue::Empty, true)),
                    Some((v,of))=> {
                        match of {
                            None =>SetResult::from_ok(set_sheet_value(&self.functions, &self.dependencies, &self.formulas, sheet, to, v, true)),
                            Some (s)=> {
                                match parse_cached_expr(&mut self.formulas, &s){
                                    Ok(expr) => set_formula_with_text(&self.functions, &mut self.dependencies, &self.formulas,sheet_idx, sheet, to, s.clone(), expr.copy(from,to)),
                                    Err(err) =>  SetResult::from_error(err),
                                }
                            }
                        }
                    },
                }*/

            },
        }
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Runtime::new()
    }
}

fn parse_cached_expr(formulas: &mut FormulaCache,formula: &str ) -> Result<Expr,EvalError> {
    match formulas.get(formula){
        Some(expr)=> Ok(expr.clone()),
        None => {
            match parse_expr(formula){
                Ok((rest,expr)) => 
                    if rest.is_empty() {
                        formulas.insert(formula.to_string(), expr.clone());
                        Ok(expr)
                    } else {
                        Err(EvalError::UnexpectedLeftover(rest.to_string()))
                    }
                Err(err) =>  Err(EvalError::IncorrectFormula(format!("{}",err))),
            }
        }
    }

} 

pub fn set_formula_with_text(functions: &FunctionLibrary, dependencies: &mut CellDependencies, formulas: &FormulaCache, sheet: &mut Sheet, id: GlobalCellID, text: String, formula: Expr) -> SetResult {
    
    runtime_check(formulas,sheet, id.cell_id, &formula)?;


    //let e=sheet.cells.cells.entry(id)
    //    .or_insert(Cell{value:CellValue::Empty,dynamic:None});
    if let Some(oldt) = sheet.cells.unset_cell_extra(&id.cell_id) {
        if let Some(olde) = formulas.get(&oldt){
            let mut orefs=HashSet::new();
            olde.get_references(&mut orefs);
            orefs.iter().for_each(|cid| {dependencies.get_mut(&id.set_cell(*cid)).map(|v| v.remove(&id));}
                );
        }
    }
    sheet.cells.set_cell_extra(id.cell_id, text);
    //e.dynamic=Some(text);
    
    let mut refs=HashSet::new();
    formula.get_references(&mut refs);
    refs.iter().for_each(|cid| {dependencies.entry(id.set_cell(*cid)).or_insert_with(HashSet::new).insert(id);});

    let new_value =runtime_sheet_eval(functions, sheet, &formula)?;

    SetResult::from_ok(set_sheet_value(functions, dependencies, formulas, sheet, id, new_value, false))

}

fn set_sheet_value(functions: &FunctionLibrary, dependencies: &CellDependencies, formulas: &FormulaCache, sheet: &mut Sheet, id: GlobalCellID, value: CellValue, remove_formula: bool) -> Vec<(CellID,Result<CellValue,EvalError>)>{
    let mut ret =vec![];
    if remove_formula {
        //let c=Cell{value:value.clone(),dynamic:None};
        sheet.cells.unset_cell_extra(&id.cell_id);
    } 
    sheet.cells.set_cell_value(id.cell_id, value.clone());
    ret.push((id.cell_id,Ok(value)));
    
   
    if let Some(hs) = dependencies.get(&id){
        for c in hs.iter().flat_map(|cid| {
            if cid.test_idx.is_none() {
            //if let Some(c) = sheet.get_cell(cid){
                if let Some(f) =sheet.cells.get_cell_extra(&cid.cell_id) {
                    if let Some(e)=formulas.get(f){
                        let r =runtime_sheet_eval(functions, sheet, e);
                        match r {
                            Ok(cv) => return set_sheet_value(functions, dependencies, formulas, sheet, *cid, cv, false),
                            Err(ee) => return vec![(cid.cell_id,Err(ee))],
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
    Range(CellRange),
    Function{name:String, args:Vec<Expr>},
    Value(CellValue),
}

impl Expr {
    pub fn get_references(&self, v: &mut HashSet<CellID>) {
        match self {
            Expr::Reference(id)=> {v.insert(*id);},
            Expr::Range(range) => range.cell_ids().into_iter().for_each(|id| {v.insert(id);}),
            Expr::Function{name:_name, args}=>args.iter().for_each(|e| e.get_references(v)),
            _ => (),
        }
    }

    pub fn copy(&self, from: CellID, to: CellID) -> Expr{
        let delta_col: i64 = to.col as i64 - from.col as i64;
        let delta_row: i64 = to.row as i64 - from.row as i64;
        self.translate(delta_col, delta_row)
    }

    fn translate(&self, delta_col: i64, delta_row: i64) -> Expr {
        match self {
            Expr::Reference(id)=> Expr::Reference(id.translate(delta_col, delta_row)),
            Expr::Range(range) => Expr::Range(range.translate(delta_col, delta_row)),
            Expr::Function{name, args}=>Expr::Function{name:name.clone(),args:args.iter().map(|e| e.translate(delta_col,delta_row)).collect()},
            Expr::Value(v)=>Expr::Value(v.clone()),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Reference(id)=> write!(f, "{}",id),
            Expr::Range(range) => write!(f, "{}",range),
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
    if let Some(f) = sheet.cells.get_cell_extra(id){
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
            Expr::Reference(id) => Ok(sheet.cells.cell_value(id)),
            Expr::Range(_) => Ok(CellValue::Empty),
            Expr::Function{name,args} =>apply_function(functions,sheet,name,args),
    }
   
}

fn apply_function(functions: &FunctionLibrary, sheet: &Sheet, name: &str, args: &[Expr]) -> Result<CellValue,EvalError> {
    if let Some(f)= functions.get(name) {
        let mut params = vec![];
        args.iter().try_for_each(|a| match a {
            Expr::Range(range) => {
                params.append(&mut sheet.cells.range_values(range));
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


#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_ref_expr() {
        let mut r=Runtime::new();

        let id = CellID::from_str("A1").unwrap();
        //let c = Cell::ProdCell{value:CellValue::Integer(1),formula:None};
        

        r.workbook.sheets[0].cells.set_cell_value(id, CellValue::Integer(1));
        assert_eq!(CellValue::Integer(1),r.workbook.sheets[0].cells.cell_value(&id));
        assert_eq!(Ok(CellValue::Integer(1)),r.eval(0, &Expr::Reference(id)));
    }

    #[test]
    fn test_range_expr() {
        let mut r=Runtime::new();

        let id1 = CellID::from_str("A1").unwrap();
        r.workbook.sheets[0].cells.set_cell_value(id1 ,CellValue::Integer(1));

        let id2 = CellID::from_str("B1").unwrap();
        r.workbook.sheets[0].cells.set_cell_value( id2,CellValue::Integer(2));
        let r1=CellRange{from:id1,to:id2};
        let r2=CellRange{from:id1,to:CellID::from_str("C1").unwrap()};
        assert_eq!(vec![CellValue::Integer(1),CellValue::Integer(2)],r.workbook.sheets[0].cells.range_values(&r2));
        assert_eq!(vec![CellValue::Integer(1),CellValue::Integer(2)],r.workbook.sheets[0].cells.range_values(&r2));
        assert_eq!(Ok(CellValue::Empty),r.eval(0, &Expr::Range(r1)));
    }

    #[test]
    fn test_function_expr(){
        let mut r=Runtime::new();

        let id1 = CellID::from_str("A1").unwrap();
        r.workbook.sheets[0].cells.set_cell_value(id1,CellValue::Integer(1));

        let id2 = CellID::from_str("B1").unwrap();
        r.workbook.sheets[0].cells.set_cell_value(id2 ,CellValue::Integer(2));

        assert_eq!(Ok(CellValue::Integer(3)),r.eval(0, &Expr::Function{name:"SUM".to_string(),args:vec![Expr::Reference(id1),Expr::Reference(id2)]}));
        assert_eq!(Ok(CellValue::Integer(3)),r.eval(0, &Expr::Function{name:"SUM".to_string(),args:vec![Expr::Range(CellRange{from:id1,to:id2})]}));

    }

    #[test]
    fn test_translate_expr(){
        let id1=CellID::from_str("A1").unwrap();
        let id2=CellID::from_str("C1").unwrap();
        let f1 = Expr::Function{name:"SUM".to_string(),args:vec![Expr::Reference(id1),Expr::Reference(id2)]};
        let f2 = Expr::Function{name:"SUM".to_string(),args:vec![Expr::Range(CellRange{from:id1,to:id2})]};
        let id3=CellID::from_str("A2").unwrap();
        let id4=CellID::from_str("C2").unwrap();
        let f3 = Expr::Function{name:"SUM".to_string(),args:vec![Expr::Reference(id3),Expr::Reference(id4)]};
        let f4 = Expr::Function{name:"SUM".to_string(),args:vec![Expr::Range(CellRange{from:id3,to:id4})]};
        assert_eq!(f3,f1.translate(0, 1));
        assert_eq!(f4,f2.translate(0, 1));
        
    }

    #[test]
    fn test_expr_change() {
        let r=&mut Runtime::new();
        
        let id1 = CellID::from_str("A1").unwrap();
        let ret1=r.set_value(0, id1, CellValue::Integer(1));
        assert_eq!(SetResult::from_ok(vec![(id1,Ok(CellValue::Integer(1)))]),ret1);
        
        let id2 = CellID::from_str("A2").unwrap();
        let ret2= r.set_value(0, id2, CellValue::Integer(1));
        assert_eq!(SetResult::from_ok(vec![(id2,Ok(CellValue::Integer(1)))]),ret2);

        let id3 = CellID::from_str("A3").unwrap();
        let ret3 = r.set_formula_str(0, id3, "SUM(A1,A2)");
        assert_eq!(SetResult::from_ok(vec![(id3,Ok(CellValue::Integer(2)))]),ret3);

        let ret1=r.set_value(0, id1, CellValue::Integer(2));
        assert_eq!(SetResult::from_ok(vec![(id1,Ok(CellValue::Integer(2))),(id3,Ok(CellValue::Integer(3)))]),ret1);
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
        
        let gid1=GlobalCellID::new(0, id1);
        let gid2=GlobalCellID::new(0, id2);
        let gid3=GlobalCellID::new(0, id3);
        

        assert_eq!(1,r.formulas.len());
        assert!(r.formulas.contains_key("SUM(A1,A2)"));
        assert_eq!(Some(&[gid3].iter().cloned().collect()),r.dependencies.get(&gid1));
        assert_eq!(Some(&[gid3].iter().cloned().collect()),r.dependencies.get(&gid2));

        let mut r2=Runtime::new();
        r2.load(r.workbook.clone());
        assert_eq!(1,r2.formulas.len());
        assert!(r2.formulas.contains_key("SUM(A1,A2)"));
        assert_eq!(Some(&[gid3].iter().cloned().collect()),r2.dependencies.get(&gid1));
        assert_eq!(Some(&[gid3].iter().cloned().collect()),r2.dependencies.get(&gid2));

        Ok(())
    }


    #[test]
    fn text_cycles(){
        let r=&mut Runtime::new();
        
        let id1 = CellID::from_str("A1").unwrap();
        let ret1 = r.set_formula(0, id1, Expr::Reference(id1));
        assert_eq!(SetResult::from_error(EvalError::CycleDetected(CellIDVec{ids:vec![id1]})), ret1);

        let id2 = CellID::from_str("A2").unwrap();

        let ret1 = r.set_formula(0, id1, Expr::Reference(id2));
        assert!(ret1.is_ok());

        let ret2 = r.set_formula(0, id2, Expr::Reference(id1));
        assert_eq!(SetResult::from_error(EvalError::CycleDetected(CellIDVec{ids:vec![id2,id1,id2]})), ret2);

    }

    #[test]
    fn test_mix()-> Result<(),EvalError>{
        let r=&mut Runtime::new();
        
        let id1 = CellID::from_str("A1").unwrap();
        r.set_value(0, id1, CellValue::Integer(1))?;
               
        let id2 = CellID::from_str("A2").unwrap();
        r.set_formula_str(0, id2, "SUM(A1,2)")?;
        assert_eq!(CellValue::Integer(3),r.workbook.sheets[0].cells.cell_value(&id2));

        Ok(())
    }

    #[test]
    fn test_custom_fn() -> Result<(),EvalError>{
        let r=&mut Runtime::new();
        
        let id1 = CellID::from_str("A1").unwrap();
        r.set_value(0, id1, CellValue::Integer(1))?;
               
        let id2 = CellID::from_str("A2").unwrap();

        r.functions.insert("ARGLENTEST".to_string(), Box::new(TestFn));

        r.set_formula_str(0, id2, "ARGLENTEST(A1,2)")?;
        assert_eq!(CellValue::Integer(2),r.workbook.sheets[0].cells.cell_value(&id2));

        Ok(())
    }

    struct TestFn;

    impl Function for TestFn {
        fn calculate(&self, args: Vec<CellValue>) -> Result<CellValue, FunctionError>{
            Ok(CellValue::Integer(args.len() as i128))
        }
    }
}