use std::collections::HashMap;
use std::str::FromStr;
use std::fmt;
//use std::marker::PhantomData;
use serde::{Deserialize, Serialize};
use serde::ser::{Serializer};
use serde::de::{Deserializer, Visitor, Error};
use thiserror::Error;
use chrono::{DateTime,Local, ParseError};

pub fn column_name(col: usize) -> String {
    let mut c = col;
    let mut v = vec![];
    loop{
        v.push(((c as u8 % 26)+b'A') as char);
        c/=26;
        if c==0{
            break;
        }
        c-=1;
    }
    v.reverse();
    v.iter().collect::<String>()
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct CellID {
    pub col: usize,
    pub row: usize,
}

impl Serialize for CellID {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

struct CellIDvisitor;

impl<'de> Visitor<'de> for CellIDvisitor {
    type Value = CellID;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("cellid")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
            E: Error, {
        CellID::from_str(v).map_err(Error::custom)
    }   

}

impl<'de> Deserialize<'de> for CellID {
    fn deserialize<D>(deserializer: D) -> Result<CellID, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(CellIDvisitor)
    }
}

impl CellID {
    pub fn next_row(&self) -> CellID {
        let nr=if self.row==std::usize::MAX {
                0
            } else {
                self.row+1
            };
        CellID{col:self.col,row:nr}
    }

    pub fn next_col(&self) -> CellID {
        let nc=if self.col==std::usize::MAX {
                0
            } else {
                self.col+1
            };
        CellID{col:nc,row:self.row}
    }

    pub fn translate(&self, delta_col: i64, delta_row: i64) -> CellID {
        let new_col=(self.col as i64 + delta_col).max(0) as usize;
        let new_row=(self.row as i64 + delta_row).max(0) as usize;
        CellID{col:new_col,row:new_row}
    }
}

impl fmt::Display for CellID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}",column_name(self.col),self.row+1)
    }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct CellIDVec {
    pub ids: Vec<CellID>,
}

impl fmt::Display for CellIDVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}",self.ids.iter().map(|id| id.to_string()).collect::<Vec<String>>().join(","))
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Error)]
pub enum CellIDParseError{
    #[error("Unexpected character: {0}")]
    Unexpected(char),
    #[error("Invalid row: {0}")]
    InvalidRow(usize),
    #[error("Invalid column: {0}")]
    InvalidCol(usize),
}

impl FromStr for CellID {
    type Err = CellIDParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut incol=true;
        let mut row = 0;
        let mut col = 0;
        for c in s.to_uppercase().chars() {
            if incol {
                if c.is_ascii_alphabetic() {
                    col= col*26 + (c as u8 - b'A' + 1) as usize;
                } else {
                    incol = false;
                }
            } 
            if !incol {
                if let Some(d)=c.to_digit(10) {
                    row=row*10 +d as usize;
                } else {
                    return Err(CellIDParseError::Unexpected(c));
                }
            }
        }
        if row<1{
            return Err(CellIDParseError::InvalidRow(row));
        }
        if col<1{
            return Err(CellIDParseError::InvalidCol(col));
        }
        Ok(CellID{col:col-1, row:row-1})
    }
}

/*pub trait CellIDOwner {
    fn id(&self) -> CellID;
}*/

#[derive(Clone, PartialOrd, PartialEq, Debug, Serialize, Deserialize)]
pub enum Cell {
    ProdCell{
        value: CellValue,
        formula: Option<String>,
    },
    TestCell {
        value: CellValue,
        expected: CellValue,
    },
}

impl Cell {
    pub fn new(value:CellValue) -> Self {
        Cell::ProdCell{value,formula:None}
    }

    pub fn get_value<'a>(&'a self) -> &'a CellValue {
        match self {
            Cell::ProdCell{value,formula:_}=>value,
            Cell::TestCell{value,expected:_}=>value,
        }
    }

    pub fn set_value(&mut self, val: CellValue) {
        match self {
            Cell::ProdCell{ref mut value,formula:_}=>*value=val,
            Cell::TestCell{ref mut value,expected:_}=>*value=val,
        }
    }
}

/*
impl CellIDOwner for Cell {
    fn id(&self) -> CellID { 
        self.id
    }
}*/

/*#[derive(Clone, PartialOrd, PartialEq, Debug, Serialize, Deserialize)]
pub struct TestCell {
   // pub id: CellID,
    pub value: CellValue,
    pub expected: CellValue,
}*/
/*
impl CellIDOwner for TestCell {
    fn id(&self) -> CellID { 
        self.id
    }
}*/

#[derive(Clone, PartialOrd, PartialEq, Debug, Serialize, Deserialize)]
pub enum CellValue {
    Text(String),
    Integer(i128),
    Float(f64),
    Boolean(bool),
    TimeStamp(DateTime<Local>),
    Date(DateTime<Local>),
    Empty,
}

impl fmt::Display for CellValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CellValue::Text(c)=>write!(f, "{}",c),
            CellValue::Integer(i)=>write!(f, "{}",i),
            CellValue::Float(c)=>write!(f, "{}",c),
            CellValue::Boolean(b)=>write!(f, "{}",b),
            CellValue::Empty=>write!(f, ""),
            CellValue::TimeStamp(dt)=>write!(f, "{}",dt.to_rfc3339()),
            CellValue::Date(dt)=>write!(f, "{}",dt.format("%F%:z")),
        }
        
    }
}


#[derive(Clone, Eq, PartialEq,Debug, Error)]
pub enum CellValueParseError{
    #[error("Invalid Integer")]
    InvalidInteger(#[source] std::num::ParseIntError),
    #[error("Invalid Float")]
    InvalidFloat(#[source] std::num::ParseFloatError),
    #[error("Invalid Boolean")]
    InvalidBoolean(#[source] std::str::ParseBoolError),
    #[error("Invalid Date/Time")]
    InvalidDateTime(#[source] ParseError),
}

impl CellValue {
    pub fn parse_similar(&self, val: &str) -> Result<CellValue,CellValueParseError> {
        match self {
            CellValue::Integer(_) => val.parse().map(CellValue::Integer).map_err(CellValueParseError::InvalidInteger),
            CellValue::Float(_) => val.parse().map(CellValue::Float).map_err(CellValueParseError::InvalidFloat),
            CellValue::Boolean(_) => val.parse().map(CellValue::Boolean).map_err(CellValueParseError::InvalidBoolean),
            CellValue::TimeStamp(_) => val.parse().map(CellValue::TimeStamp).map_err(CellValueParseError::InvalidDateTime),
            CellValue::Date(_) => val.parse().map(CellValue::Date).map_err(CellValueParseError::InvalidDateTime),
            CellValue::Empty => Ok(CellValue::Empty),
            _ => Ok(CellValue::Text(val.to_owned())),
        }
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct CellRange {
    pub from: CellID,
    pub to: CellID,
}

impl CellRange {
    pub fn cell_ids(&self) -> Vec<CellID> {
        (self.from.row ..= self.to.row).flat_map(|r| (self.from.col ..= self.to.col).map(move |c| CellID{row:r,col:c})).collect()
    }

    pub fn translate(&self, delta_col: i64, delta_row: i64) -> CellRange {
        CellRange{from:self.from.translate(delta_col, delta_row),to:self.to.translate(delta_col, delta_row)}
    }
}

impl fmt::Display for CellRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}",self.from,self.to)
    }
}


#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Error)]
pub enum CellRangeParseError{
    #[error("Unexpected character: {0}")]
    Unexpected(char),
    #[error("Invalid row: {0}")]
    InvalidRow(usize),
    #[error("Invalid column: {0}")]
    InvalidCol(usize),
    #[error("Invalid range")]
    InvalidRange,
}

impl FromStr for CellRange {
    type Err = CellRangeParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut incol=true;
        let mut row = 0;
        let mut col = 0;
        let mut from = None;
        let mut to = None;
        for c in s.to_uppercase().chars() {
            if incol {
                if c.is_ascii_alphabetic() {
                    col= col*26 + (c as u8 - b'A' + 1) as usize;
                } else {
                    incol = false;
                }
            } 
            if !incol {
                if let Some(d)=c.to_digit(10) {
                    row=row*10 +d as usize;
                } else if c==':' {
                    if row<1{
                        return Err(CellRangeParseError::InvalidRow(row));
                    }
                    if col<1{
                        return Err(CellRangeParseError::InvalidCol(col));
                    }
                    if from.is_none(){
                        from=Some(CellID{col:col-1, row:row-1});
                        row=0;
                        col=0;
                        incol=true;
                    } else {
                        return Err(CellRangeParseError::Unexpected(c));
                    }
                } else {
                    return Err(CellRangeParseError::Unexpected(c));
                }
            }
        }
        if row<1{
            return Err(CellRangeParseError::InvalidRow(row));
        }
        if col<1{
            return Err(CellRangeParseError::InvalidCol(col));
        }
        if to.is_none(){
            to=Some(CellID{col:col-1, row:row-1});
        }
        if let Some(f) = from {
            if let Some(t) = to {
                return Ok (CellRange{from:f,to:t});
            }
        }
        Err(CellRangeParseError::InvalidRange)
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct CellMap<T>{
    pub metadata: Metadata,
    pub values: HashMap<CellID,CellValue>,
    pub extras: HashMap<CellID,T>,
}

impl <T> CellMap<T> {
    pub fn get_cell_value<'a>(&'a self, id: &CellID ) -> Option<&'a CellValue> {
        self.values.get(id)
    }

    pub fn get_cell_extra<'a>(&'a self, id: &CellID ) -> Option<&'a T> {
        self.extras.get(id)
    }


    pub fn set_cell_value(&mut self, id: CellID, value: CellValue) {
        //let myid=*id;
        //self.cells.entry(*id).or_insert(Cell{value:CellValue::Empty,dynamic}).value=value;
        self.values.insert(id,value);
        self.calc_size(&id);
    }

    pub fn unset_cell_value(&mut self, id: &CellID) -> Option<CellValue> {
        //let myid=*id;
        //self.cells.entry(*id).or_insert(Cell{value:CellValue::Empty,dynamic}).value=value;
        self.values.remove(id)
    }

    pub fn set_cell_extra(&mut self, id: CellID, extra: T) {
        //let myid=*id;
        //self.cells.entry(*id).or_insert(Cell{value:CellValue::Empty,dynamic}).value=value;
        self.extras.insert(id,extra);
        self.calc_size(&id);
    }

    pub fn unset_cell_extra(&mut self, id: &CellID) -> Option<T> {
        //let myid=*id;
        //self.cells.entry(*id).or_insert(Cell{value:CellValue::Empty,dynamic}).value=value;
        self.extras.remove(id)
    }


    /*pub fn set_cell_value(&mut self, id: &CellID, value: CellValue, dynamic: T) {
        //let myid=*id;
        self.cells.entry(*id).or_insert(Cell{value:CellValue::Empty,dynamic}).value=value;
        self.calc_size(id);
    }*/

    /*pub fn set_cell(&mut self, id: CellID, cell: Cell) -> CellID {
        //let id = cell.id;
        self.cells.insert(id.clone(),cell);
        self.calc_size(&id);
        id
    }*/

    fn calc_size(&mut self, id: &CellID) {
        let sd=self.metadata.size();
        let nx = if sd.0<=id.col {
            id.col+1
        } else {
            sd.0
        };
        let ny = if sd.1<=id.row {
            id.row+1
        } else {
            sd.1
        };
        self.metadata.size=(nx,ny);
    }

    
    pub fn cell_value(&self, id: &CellID) -> CellValue {
        self.get_cell_value(id).map(|c| c.clone()).unwrap_or(CellValue::Empty)
    }

    pub fn range_values(&self, range: &CellRange) -> Vec<CellValue> {
        range.cell_ids().iter().map(|id|  self.cell_value(id)).filter(|v| !matches!(v, CellValue::Empty)).collect()
    }
}

pub type ProdCellMap = CellMap<String>;
pub type TestCellMap = CellMap<CellValue>;

/*
impl ProdCellMap {
   
    pub fn set_cell_value(&mut self, id: &CellID, value: CellValue, formula: Option<String>) {
        //let myid=*id;
        self.cells.entry(*id).or_insert(Cell::ProdCell{value:CellValue::Empty,formula}).set_value(value);
        self.calc_size(id);
    }
}

impl TestCellMap {
   
    pub fn set_cell_value(&mut self, id: &CellID, value: CellValue, expected: CellValue) {
        //let myid=*id;
        self.cells.entry(*id).or_insert(Cell::TestCell{value:CellValue::Empty,expected}).set_value(value);
        self.calc_size(id);
    }
}*/

/*
impl<T:Serialize> Serialize for CellMap<T>{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer, {
        let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
        for e in self.0.values() {
            seq.serialize_element(e)?;
        }
        seq.end()
    }
}

struct CellMapVisitor<T>{
    data: PhantomData<T>,
}

impl<'de, T:CellIDOwner + Deserialize<'de>> Visitor<'de> for CellMapVisitor<T> {
    type Value = CellMap<T>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("id: cellid")
    }

    fn visit_seq<S>(self, mut access: S) -> Result<Self::Value, S::Error>
    where
        S: SeqAccess<'de>,
    {
        let mut items = HashMap::with_capacity(access.size_hint().unwrap_or(0));
        while let Some(cell) = access.next_element::<T>()? {
            items.insert(cell.id(), cell);
        }
        Ok(CellMap(items))
    }
}

impl<'de, T:CellIDOwner + Deserialize<'de>> Deserialize<'de> for CellMap<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_seq(CellMapVisitor{data:PhantomData})
    }
}*/


#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct GlobalCellID {
    pub sheet_idx: usize,
    pub test_idx: Option<usize>,
    pub cell_id: CellID,
}

impl GlobalCellID {
    pub fn new(sheet_idx: usize, cell_id: CellID)-> GlobalCellID {
        Self {sheet_idx,test_idx:None, cell_id}
    }
    pub fn new_test(sheet_idx: usize, test_idx: usize, cell_id: CellID)-> GlobalCellID {
        Self {sheet_idx,test_idx:Some(test_idx), cell_id}
    }
    
    pub fn set_cell(&self, cell_id: CellID)-> GlobalCellID {
        Self {sheet_idx:self.sheet_idx,test_idx:self.test_idx, cell_id}
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Sheet {
    pub cells:ProdCellMap,
    pub tests:Vec<TestCellMap>,
}

impl Sheet {
    pub fn new()-> Self {
        Sheet{
            cells: CellMap{metadata: Metadata::new(),values:HashMap::new(),extras:HashMap::new()},
            tests:Vec::new(),
        }
    }

    pub fn get_cell_value<'a>(&'a self, id: GlobalCellID) -> Option<&'a CellValue> {
        match id.test_idx {
            None=> self.cells.get_cell_value(&id.cell_id),
            Some(idx)=>self.tests[idx].get_cell_value(&id.cell_id),
        }
    }
}

impl Default for Sheet {
    fn default() -> Self {
        Sheet::new()
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Workbook {
    pub sheets: Vec<Sheet>,
}

impl Workbook {
    pub fn new() -> Self {
        Workbook{sheets:vec![Sheet::new()]}
    }

    pub fn new_sheet(&mut self) -> usize {
        self.sheets.push(Sheet::new());
        self.sheets.len()-1
    } 

    pub fn get_cell_value<'a,T>(&'a self, id: GlobalCellID) -> Option<&'a CellValue> {
        let s = &self.sheets[id.sheet_idx];
        s.get_cell_value(id)
    }
}

impl Default for Workbook {
    fn default() -> Self {
        Workbook::new()
    }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct Metadata {
    pub name: String,
    pub headers: usize, 
    size: (usize,usize),
}


impl Metadata {
    pub fn new()-> Self {
        Metadata{
            name:String::new(),
            headers:0,
            size:(0,0)}
    }

    pub fn size(&self) -> (usize,usize) {
        self.size
    }
}

impl Default for Metadata {
    fn default() -> Self {
        Metadata::new()
    }
}
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct InputDescription {
    pub headers: usize,
    pub template: Vec<CellValue>,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_cell_id() {
        assert_eq!(Ok(CellID{row:0,col:0}), CellID::from_str("A1"));
        assert_eq!(Ok(CellID{row:0,col:1}), CellID::from_str("B1"));
        assert_eq!(Ok(CellID{row:0,col:25}), CellID::from_str("Z1"));
        assert_eq!(Ok(CellID{row:0,col:26}), CellID::from_str("AA1"));
        assert_eq!(Ok(CellID{row:0,col:51}), CellID::from_str("AZ1"));
        assert_eq!(Ok(CellID{row:9,col:0}), CellID::from_str("A10"));
        assert_eq!(Ok(CellID{row:99,col:1}), CellID::from_str("B100"));
        assert_eq!(Ok(CellID{row:24,col:25}), CellID::from_str("Z25"));
        assert_eq!(Ok(CellID{row:87,col:26}), CellID::from_str("AA88"));
        assert_eq!(Ok(CellID{row:344,col:51}), CellID::from_str("AZ345"));
        
        assert_eq!(Err(CellIDParseError::InvalidRow(0)),CellID::from_str("hello"));
        assert_eq!(Err(CellIDParseError::InvalidCol(0)),CellID::from_str("123"));
        assert_eq!(Err(CellIDParseError::Unexpected('-')),CellID::from_str("A-1"));
    }

    #[test]
    fn parse_display_cell_id(){
        let strs=vec!["A1","B1","Z1","AA1","AZ1","A10","B100","Z25","AA88","AZ345"];
        for str in strs.into_iter() {
            assert_eq!(str.to_owned(), format!("{}",CellID::from_str(str).unwrap()));
        }
    }

    #[test]
    fn translate_cellid(){
        assert_eq!(CellID::from_str("A1"),CellID::from_str("A1").map(|c| c.translate(-1,-1)));
        assert_eq!(CellID::from_str("A2"),CellID::from_str("A1").map(|c| c.translate(0,1)));
        assert_eq!(CellID::from_str("B1"),CellID::from_str("A1").map(|c| c.translate(1,0)));
        assert_eq!(CellID::from_str("A1"),CellID::from_str("A2").map(|c| c.translate(0,-1)));
        assert_eq!(CellID::from_str("A1"),CellID::from_str("B1").map(|c| c.translate(-1,0)));
    }

    #[test]
    fn test_parse_range(){
        assert_eq!(Ok(CellRange{from:CellID{row:0,col:0},to:CellID{row:0,col:1}}), CellRange::from_str("A1:B1"));
    }

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
        assert_eq!(expected.iter().map(|s| CellID::from_str(s).unwrap()).collect::<Vec<CellID>>(),CellRange{from:CellID::from_str(from).unwrap(),to: CellID::from_str(to).unwrap()}.cell_ids());
    }

    #[test]
    fn translate_rangeid(){
        assert_eq!(CellRange::from_str("B1:B3"),CellRange::from_str("A1:A3").map(|c| c.translate(1,0)));
        assert_eq!(CellRange::from_str("A2:C2"),CellRange::from_str("A1:C1").map(|c| c.translate(0,1)));

    }

    #[test]
    fn test_sheet_size() {
        let mut s=Sheet::new();
        assert_eq!((0,0),s.cells.metadata.size());
        s.cells.set_cell_value(CellID::from_str("A1").unwrap(), CellValue::Text("Name".to_string()));
        assert_eq!((1,1),s.cells.metadata.size());
        s.cells.set_cell_value( CellID::from_str("B1").unwrap(), CellValue::Text("Value".to_string()));
        assert_eq!((2,1),s.cells.metadata.size());
        s.cells.set_cell_value(CellID::from_str("A2").unwrap(),CellValue::Integer(1));
        assert_eq!((2,2),s.cells.metadata.size());
        s.cells.set_cell_value( CellID::from_str("B2").unwrap(),CellValue::Integer(2));
        assert_eq!((2,2),s.cells.metadata.size());
        s.cells.set_cell_value(CellID::from_str("A3").unwrap(),CellValue::Integer(3));
        assert_eq!((2,3),s.cells.metadata.size());

    }
}