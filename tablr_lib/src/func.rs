use crate::types::*;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Clone, PartialOrd, PartialEq, Debug, Error)]
pub enum FunctionError {
    #[error("Invalid argument at index {0}: {1}")]
    InvalidArgs(usize,CellValue),
    #[error("Unexpected intermediate value: {0}")]
    UnexpectedValue(CellValue),
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

impl <T:Binary> Function for T {
    fn calculate(&self, args: Vec<CellValue>) -> Result<CellValue, FunctionError> {
        binary_function(self, args)
     }
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
}