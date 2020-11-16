use crate::types::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Clone, PartialOrd, PartialEq, Debug, Error)]
pub enum FunctionError {
    #[error("Invalid argument at index {0}: {1}")]
    InvalidArgs(usize,CellValue),
    #[error("Unexpected intermediate value: {0}")]
    UnexpectedValue(CellValue),
    #[error("Wrong number of arguments: {0}, expected at least {1}")]
    WrongNumberOfArgument(usize,usize),
    #[error("Unknown function error")]
    Unknown,
}

pub trait Function {
    fn calculate(&self, args: Vec<CellValue>) -> Result<CellValue, FunctionError>;
}

pub trait Binary: Sized {
    fn id(&self) -> CellValue;

    fn single(&self, cv: CellValue)-> Option<CellValue>; 

    fn op(&self, cv1: CellValue, cv2: CellValue) -> Option<CellValue>; 
}


fn binary_function<B>(binary: &B, args: Vec<CellValue>) -> Result<CellValue, FunctionError> where B: Binary {
    if args.is_empty() {
        return Ok(binary.id());
    }
    let mut ret = None;
    for (idx,cv) in args.into_iter().enumerate() {
        match match ret {
            None => binary.single(cv.clone()),
            Some(cv1) => binary.op(cv1,cv.clone()), 
        } {
            Some(ncv) => ret=Some(ncv),
            None => return Err(FunctionError::InvalidArgs(idx,cv)),
        }
    }
    ret.ok_or(FunctionError::Unknown)
}


pub fn built_in_functions() -> HashMap<String, Box<dyn Function>> {
    let mut m: HashMap<String, Box<dyn Function>> = HashMap::new();
    m.insert("SUM".to_string(), Box::new(Add));
    m.insert("ADD".to_string(), Box::new(Add));
    m.insert("MINUS".to_string(), Box::new(Substract));
    m.insert("SUBSTRACT".to_string(), Box::new(Substract));
    m.insert("MULTIPLY".to_string(), Box::new(Multiply));
    m.insert("TIMES".to_string(), Box::new(Multiply));
    m.insert("DIVIDE".to_string(), Box::new(Divide));
    m.insert("AVERAGE".to_string(), Box::new(Average));
    m.insert("AVG".to_string(), Box::new(Average));
    m.insert("MAX".to_string(), Box::new(Max));
    m.insert("MIN".to_string(), Box::new(Min));
    m
}

struct Add;

impl Binary for Add {
    fn id(&self) -> CellValue {
        CellValue::Integer(0)
    }

    fn single(&self, cv: CellValue)-> Option<CellValue> {
        match cv {
            CellValue::Integer(_i) => Some(cv),
            CellValue::Float(_f) => Some(cv),
            _ => None,
        }
    } 

    fn op(&self, cv1: CellValue, cv2: CellValue) -> Option<CellValue>{
        match (cv1,cv2){
            (CellValue::Integer(i1),CellValue::Integer(i2))=>Some(CellValue::Integer(i1+i2)),
            (CellValue::Float(f1),CellValue::Integer(i2))=>Some(CellValue::Float(f1+i2 as f64)),
            (CellValue::Integer(i1),CellValue::Float(f2))=>Some(CellValue::Float(i1 as f64 +f2)),
            (CellValue::Float(f1),CellValue::Float(f2))=>Some(CellValue::Float(f1+f2)),
            _ => None,
        }
    }
}

impl Function for Add {
    fn calculate(&self, args: Vec<CellValue>) -> Result<CellValue, FunctionError> {
        binary_function(self, args)
     }
}


struct Substract;

impl Binary for Substract {
    fn id(&self) -> CellValue {
        CellValue::Integer(0)
    }

    fn single(&self, cv: CellValue)-> Option<CellValue> {
        match cv {
            CellValue::Integer(_i) => Some(cv),
            CellValue::Float(_f) => Some(cv),
            _ => None,
        }
    } 

    fn op(&self, cv1: CellValue, cv2: CellValue) -> Option<CellValue>{
        match (cv1,cv2){
            (CellValue::Integer(i1),CellValue::Integer(i2))=>Some(CellValue::Integer(i1-i2)),
            (CellValue::Float(f1),CellValue::Integer(i2))=>Some(CellValue::Float(f1-i2 as f64)),
            (CellValue::Integer(i1),CellValue::Float(f2))=>Some(CellValue::Float(i1 as f64 -f2)),
            (CellValue::Float(f1),CellValue::Float(f2))=>Some(CellValue::Float(f1-f2)),
            _ => None,
        }
    }
}

impl Function for Substract {
    fn calculate(&self, args: Vec<CellValue>) -> Result<CellValue, FunctionError> {
        binary_function(self, args)
     }
}

struct Multiply;

impl Binary for Multiply {
    fn id(&self) -> CellValue {
        CellValue::Integer(0)
    }

    fn single(&self, cv: CellValue)-> Option<CellValue> {
        match cv {
            CellValue::Integer(_i) => Some(cv),
            CellValue::Float(_f) => Some(cv),
            _ => None,
        }
    } 

    fn op(&self, cv1: CellValue, cv2: CellValue) -> Option<CellValue>{
        match (cv1,cv2){
            (CellValue::Integer(i1),CellValue::Integer(i2))=>Some(CellValue::Integer(i1*i2)),
            (CellValue::Float(f1),CellValue::Integer(i2))=>Some(CellValue::Float(f1*i2 as f64)),
            (CellValue::Integer(i1),CellValue::Float(f2))=>Some(CellValue::Float(i1 as f64 *f2)),
            (CellValue::Float(f1),CellValue::Float(f2))=>Some(CellValue::Float(f1*f2)),
            _ => None,
        }
    }
}

impl Function for Multiply {
    fn calculate(&self, args: Vec<CellValue>) -> Result<CellValue, FunctionError> {
        binary_function(self, args)
     }
}

struct Divide;

impl Binary for Divide {
    fn id(&self) -> CellValue {
        CellValue::Integer(0)
    }

    fn single(&self, cv: CellValue)-> Option<CellValue> {
        match cv {
            CellValue::Integer(_i) => Some(cv),
            CellValue::Float(_f) => Some(cv),
            _ => None,
        }
    } 

    fn op(&self, cv1: CellValue, cv2: CellValue) -> Option<CellValue>{
        match (cv1,cv2){
            (CellValue::Integer(i1),CellValue::Integer(i2))=>Some(CellValue::Integer(i1/i2)),
            (CellValue::Float(f1),CellValue::Integer(i2))=>Some(CellValue::Float(f1/i2 as f64)),
            (CellValue::Integer(i1),CellValue::Float(f2))=>Some(CellValue::Float(i1 as f64 /f2)),
            (CellValue::Float(f1),CellValue::Float(f2))=>Some(CellValue::Float(f1/f2)),
            _ => None,
        }
    }
}

impl Function for Divide {
    fn calculate(&self, args: Vec<CellValue>) -> Result<CellValue, FunctionError> {
        binary_function(self, args)
     }
}

struct Average;

impl Function for Average {
    fn calculate(&self, args: Vec<CellValue>) -> Result<CellValue, FunctionError>{
        let l = args.len() as f64;
        match Add.calculate(args)?{
            CellValue::Integer(i) => Ok(CellValue::Float(i as f64 / l)),
            CellValue::Float(f) => Ok(CellValue::Float(f / l)),
            cv => Err(FunctionError::UnexpectedValue(cv)),
        }
    }

}

pub trait NumericFold : Sized{
    fn int_cmp(&self, i1: i128, i2: i128) -> i128;
    fn float_cmp(&self, f1: f64, f2: f64) -> f64;
    
}

fn fold_calculate<N>(nb: &N, args: Vec<CellValue>) -> Result<CellValue, FunctionError> where N: NumericFold{
    let l = args.len();
    if l==0 {
        return Err(FunctionError::WrongNumberOfArgument(0,1));
    }
    let mut int_ret: Option<i128>=None;
    let mut float_ret: Option<f64>=None;
    for (idx,v) in args.into_iter().enumerate() {
        match v {
            CellValue::Integer(i) => {
                if let Some(m) = int_ret {
                    int_ret=Some(nb.int_cmp(m,i));
                } else if let Some(m) = float_ret {
                    float_ret=Some(nb.float_cmp(m, i as f64));
                } else {
                    int_ret=Some(i);
                }
            },
            CellValue::Float(f) => {
                if let Some(m) = int_ret {
                    float_ret=Some(nb.float_cmp(m as f64, f));
                    int_ret=None;
                } else if let Some(m) = float_ret {
                    float_ret=Some(nb.float_cmp(m, f));
                } else {
                    float_ret=Some(f);
                }
            },
            CellValue::Empty => {},
            cv => return Err(FunctionError::InvalidArgs(idx,cv)),
        }
    }
    if let Some(m) = int_ret {
        Ok(CellValue::Integer(m))
    } else if let Some(m) = float_ret {
        Ok(CellValue::Float(m))
    } else {
        Err(FunctionError::WrongNumberOfArgument(0,1))
    }
}


struct Max;

impl NumericFold for Max {
    fn int_cmp(&self, i1: i128, i2: i128) -> i128{
        i1.max(i2)
    }
    fn float_cmp(&self, f1: f64, f2: f64) -> f64{
        f1.max(f2)
    }
    
}

impl Function for Max {
    fn calculate(&self, args: Vec<CellValue>) -> Result<CellValue, FunctionError> {
       fold_calculate(self, args)
    }
}

struct Min;

impl NumericFold for Min {
    fn int_cmp(&self, i1: i128, i2: i128) -> i128{
        i1.min(i2)
    }
    fn float_cmp(&self, f1: f64, f2: f64) -> f64{
        f1.min(f2)
    }
    
}

impl Function for Min {
    fn calculate(&self, args: Vec<CellValue>) -> Result<CellValue, FunctionError> {
       fold_calculate(self, args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add(){
        let s = Add;
        assert_eq!(Ok(CellValue::Integer(3)), s.calculate(vec![CellValue::Integer(1),CellValue::Integer(2)]));
        assert_eq!(Ok(CellValue::Float(3.0)), s.calculate(vec![CellValue::Float(1.0),CellValue::Float(2.0)]));
        assert_eq!(Ok(CellValue::Float(3.0)), s.calculate(vec![CellValue::Integer(1),CellValue::Float(2.0)]));
        assert_eq!(Ok(CellValue::Float(3.0)), s.calculate(vec![CellValue::Float(1.0),CellValue::Integer(2)]));
        assert_eq!(Err(FunctionError::InvalidArgs(1,CellValue::Text("a".to_string()))), s.calculate(vec![CellValue::Integer(1),CellValue::Text("a".to_string())]));
        assert_eq!(Err(FunctionError::InvalidArgs(1,CellValue::Text("a".to_string()))), s.calculate(vec![CellValue::Float(3.0),CellValue::Text("a".to_string())]));
        assert_eq!(Ok(CellValue::Integer(0)), s.calculate(vec![]));
    }

    #[test]
    fn test_average(){
        let s= Average;
        assert_eq!(Ok(CellValue::Float(1.5)), s.calculate(vec![CellValue::Float(1.0),CellValue::Float(2.0)]));
        assert_eq!(Err(FunctionError::InvalidArgs(1,CellValue::Text("a".to_string()))), s.calculate(vec![CellValue::Integer(1),CellValue::Text("a".to_string())]));
    }

    #[test]
    fn test_min_max(){
       assert_eq!(Ok(CellValue::Float(2.0)), Max.calculate(vec![CellValue::Integer(1),CellValue::Empty,CellValue::Float(2.0)]));
       assert_eq!(Ok(CellValue::Float(1.0)), Min.calculate(vec![CellValue::Integer(1),CellValue::Empty,CellValue::Float(2.0)]));
       assert_eq!(Ok(CellValue::Integer(2)), Max.calculate(vec![CellValue::Integer(1),CellValue::Empty,CellValue::Integer(2)]));
       assert_eq!(Ok(CellValue::Integer(1)), Min.calculate(vec![CellValue::Integer(1),CellValue::Empty,CellValue::Integer(2)]));
       
    }
}