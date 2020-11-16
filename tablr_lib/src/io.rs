use crate::types::*;
use std::fs::File;
use std::fmt::Display;
use std::io::{BufReader, BufWriter,Write};
use std::path::Path;
use serde_json::{to_writer, from_reader};
use csv::{WriterBuilder, StringRecord,ReaderBuilder};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum IOError {
    #[error("Could not create workbook save file {0}")]
    CreateError(String,#[source] std::io::Error),
    #[error("Could not create CSV save file {0}")]
    CreateCSVError(String,#[source] std::io::Error),
    #[error("Could not serialize workbook to {0}")]
    SerializeError(String,#[source] serde_json::Error),
    #[error("Error while writing CSV record")]
    WriteCSVError(#[source] csv::Error),
    #[error("Could not open workbook load file {0}")]
    OpenError(String,#[source] std::io::Error),
    #[error("Could not deserialize workbook from {0}")]
    DeserializeError(String,#[source] serde_json::Error),
    #[error("Could not open CSV file {0}")]
    OpenCSVError(String,#[source] csv::Error),
    #[error("Could not deserialize sheet from CSV {0}")]
    DeserializeCSVError(String,#[source] csv::Error),
    #[error("Could not deserialize cell from CSV field {0}")]
    ParseCSVFieldError(CellID,#[source] CellValueParseError),
}


pub fn save<P: AsRef<Path> + Display>(workbook: &Workbook, path: &P) -> Result<(),IOError> {
    let f= File::create(path).map_err(|e| IOError::CreateError(format!("{}",path),e))?;
    let w=BufWriter::new(f);
    to_writer(w, workbook).map_err(|e| IOError::SerializeError(format!("{}", path),e))?;
    Ok(())
}

pub fn save_csv<P: AsRef<Path> + Display>(sheet: &Sheet, path: &P) -> Result<(),IOError> {
    let f= File::create(path).map_err(|e| IOError::CreateCSVError(format!("{}",path),e))?;
    let w=BufWriter::new(f);
    save_csv_writer(sheet,w)
}

pub fn save_csv_writer<W: Write>(sheet: &Sheet, w: W) -> Result<(),IOError> {
    let mut wtr = WriterBuilder::new().from_writer(w);
    let mut row = Vec::with_capacity(sheet.cells.metadata.size().0);
    
    for r in 0..sheet.cells.metadata.size().1 {
        row.clear();
        for c in 0..sheet.cells.metadata.size().0 {
            row.push(match sheet.cells.cells.get(&CellID{col:c,row:r}) {
                Some(cell) =>format!("{}",cell.value),
                None =>String::new(),
            });
        }
        wtr.write_record(&row).map_err( IOError::WriteCSVError)?;
    }
    Ok(())
}


pub fn load<P: AsRef<Path>+Display>(path:&P) -> Result<Workbook,IOError> {
    let f = File::open(path).map_err(|e| IOError::OpenError(format!("{}",path),e))?;
    let r = BufReader::new(f);
    Ok(from_reader(r).map_err(|e| IOError::DeserializeError(format!("{}", path),e))?)
    
}

pub fn load_csv<P: AsRef<Path>+Display>(path:&P, desc: &InputDescription) -> Result<Sheet,IOError> {
    let mut rdr = ReaderBuilder::new().has_headers(false).flexible(true).from_path(path).map_err(|e| IOError::OpenCSVError(format!("{}",path),e))?;
    let mut sheet = Sheet::new();
    sheet.cells.metadata.headers=desc.headers;
    
    let def = CellValue::Text(String::new());
    for (row,result) in rdr.records().enumerate() {
        let record: StringRecord = result.map_err(|e| IOError::DeserializeCSVError(format!("{}", path),e))?;
        for (col,field) in record.iter().enumerate(){
            let id = CellID{col,row};
            let cell_value= if field.is_empty() {
                CellValue::Empty
            } else {
                let template_value=
                    if row<desc.headers {
                        &def
                    } else {
                        desc.template.get(col).unwrap_or(&def)
                    };
                
                template_value.parse_similar(field).map_err(|e| IOError::ParseCSVFieldError(id,e))?
            };
            sheet.cells.set_cell_value(&id, cell_value, None);
        }
    }
    Ok(sheet)
}



#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{remove_file, read};
    use std::str::FromStr;

    #[test]
    fn test_save_load()-> Result<(),IOError> {
        let mut w=Workbook::new();
        w.sheets[0].cells.set_cell(CellID::from_str("A1").unwrap(),Cell::new(CellValue::Text("Name".to_string())));
        w.sheets[0].cells.set_cell( CellID::from_str("B1").unwrap(),Cell::new(CellValue::Text("Value".to_string())));
        w.sheets[0].cells.set_cell(CellID::from_str("A2").unwrap(),Cell::new(CellValue::Integer(1)));
        w.sheets[0].cells.set_cell( CellID::from_str("B2").unwrap(),Cell::new(CellValue::Integer(2)));
        w.sheets[0].cells.metadata.headers=1;

        let p="workbook.tablr.json";
        save(&w, &p)?;
        let w2=load(&p)?;
        assert_eq!(w,w2);
        assert!(remove_file(&p).is_ok());
        Ok(())
    }

    #[test]
    fn test_save_csv() -> Result<(),IOError> {
        let mut s=Sheet::new();
        s.cells.metadata.headers=1;
        s.cells.set_cell(CellID::from_str("A1").unwrap(),Cell::new(CellValue::Text("Name".to_string())));
        s.cells.set_cell( CellID::from_str("B1").unwrap(),Cell::new(CellValue::Text("Value".to_string())));
        s.cells.set_cell(CellID::from_str("A2").unwrap(),Cell::new(CellValue::Integer(1)));
        s.cells.set_cell( CellID::from_str("B2").unwrap(),Cell::new(CellValue::Integer(2)));
        s.cells.set_cell(CellID::from_str("A3").unwrap(),Cell::new(CellValue::Integer(3)));
       
        let p="workbook.tablr.csv";
        save_csv(&s, &p)?;
        let data = String::from_utf8(read(&p).unwrap()).unwrap();
        assert_eq!(data, "Name,Value\n1,2\n3,\n");

        let template = vec![CellValue::Integer(0),CellValue::Integer(0),CellValue::Integer(1)];
        let s2= load_csv(&p, &InputDescription{headers:1, template})?;
        assert_eq!(s.cells.metadata,s2.cells.metadata);
        assert_eq!(s.cells.get_cell(&"A1".parse().unwrap()),s2.cells.get_cell(&"A1".parse().unwrap()));
        assert_eq!(s.cells.get_cell(&"A2".parse().unwrap()),s2.cells.get_cell(&"A2".parse().unwrap()));
        assert_eq!(s.cells.get_cell(&"A3".parse().unwrap()),s2.cells.get_cell(&"A3".parse().unwrap()));
        assert_eq!(s.cells.get_cell(&"B1".parse().unwrap()),s2.cells.get_cell(&"B1".parse().unwrap()));
        assert_eq!(s.cells.get_cell(&"B2".parse().unwrap()),s2.cells.get_cell(&"B2".parse().unwrap()));
        assert!(remove_file(&p).is_ok());
        Ok(())
    }


}