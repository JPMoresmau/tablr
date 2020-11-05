use tablr_lib::*;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag},
    character::complete::{space1, space0, digit1},
    combinator::{eof},
    multi::many1,
  };
use chrono::{Local};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Command {
    Load(String),
    Save(Option<String>),
    LoadCSV(String,InputDescription),
    SaveCSV(Option<String>),
    Quit,
    Help,
    Choose(CellID),
    SetValue(CellValue),
    SetFormula(String),
    Error(String),
}

pub fn parse_command(input: &str) -> IResult<&str, Command> {
    alt((parse_io,parse_quit,parse_help,parse_formula,parse_choose,parse_value))(input)
}

fn parse_io(input: &str) -> IResult<&str, Command> {
    alt((parse_load_csv,parse_save_csv,parse_load,parse_save))(input)
}

fn parse_load(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("load"),tag("l")))(input)?;
    let (input,_)=space1(input)?;
    Ok(("",Command::Load(input.to_string())))
}

fn parse_save(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("save"),tag("s")))(input)?;
    let (input,_)=space0(input)?;
    let s = input.to_string();
    let op = if s.is_empty() {
        None
    } else {
        Some(s)
    };
    Ok(("",Command::Save(op)))
}

fn parse_load_csv(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("lc"),tag("loadcsv")))(input)?;
    let (input,_)=space1(input)?;
    let (input,headers)=digit1(input).map(|(i,s)| (i,s.parse().unwrap()))?;
    let (input,_)=space1(input)?;
    let (input,template)=many1(parse_def_value)(input)?;
    Ok(("",Command::LoadCSV(input.to_string(),InputDescription{headers,template})))
}

fn parse_save_csv(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("sc"),tag("savecsv")))(input)?;
    let (input,_)=space0(input)?;
    let s = input.to_string();
    let op = if s.is_empty() {
        None
    } else {
        Some(s)
    };
    Ok(("",Command::SaveCSV(op)))
}

fn parse_quit(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("quit"),tag("q")))(input)?;
    let (input,_)=space0(input)?;
    eof(input)?;
    Ok(("",Command::Quit))
}

fn parse_help(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("help"),tag("h"),tag("?")))(input)?;
    Ok((input,Command::Help))
}

fn parse_formula(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("formula"),tag("f")))(input)?;
    let (input,_)=space0(input)?;
    Ok(("",Command::SetFormula(input.to_string())))
}

fn parse_choose(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("choose"),tag("c")))(input)?;
    let (input,_)=space1(input)?;
    let (input,cell_id) = parse_id(input)?;
    Ok((input,Command::Choose(cell_id)))
}

fn parse_value(input: &str) -> IResult<&str, Command> {
    alt((parse_text_value,parse_integer_value,parse_float_value,parse_bool_value,parse_date_value,parse_time_value))(input)
}

fn parse_text_value(input: &str) -> IResult<&str, Command> {
    let (input,def)=parse_def_text_value(input)?;
    parse_value_with_default(input, def)
}

fn parse_integer_value(input: &str) -> IResult<&str, Command> {
    let (input,def)=parse_def_integer_value(input)?;
    parse_value_with_default(input, def)
}

fn parse_float_value(input: &str) -> IResult<&str, Command> {
    let (input,def)=parse_def_float_value(input)?;
    parse_value_with_default(input, def)
}

fn parse_bool_value(input: &str) -> IResult<&str, Command> {
    let (input,def)=parse_def_bool_value(input)?;
    parse_value_with_default(input, def)
    
}

fn parse_date_value(input: &str) -> IResult<&str, Command> {
    let (input,def)=parse_def_date_value(input)?;
    parse_value_with_default(input, def)
}

fn parse_time_value(input: &str) -> IResult<&str, Command> {
    let (input,def)=parse_def_time_value(input)?;
    parse_value_with_default(input, def)
}


fn parse_value_with_default(input: &str,def: CellValue) -> IResult<&str, Command> {
    if input.is_empty() {
        return Ok(("",Command::SetValue(def)));
    }
    let (input,_)=space1(input)?;
    if input.is_empty() {
        Ok(("",Command::SetValue(def)))
    } else {
        Ok(("",match def.parse_similar(input){
            Ok(cv) => Command::SetValue(cv),
            Err(err) => Command::Error(format!("{}",err)),
        }))
    }
}

fn parse_def_value(input: &str) -> IResult<&str, CellValue> {
    let (input,cv)=alt((parse_def_text_value,parse_def_integer_value,parse_def_float_value,parse_def_bool_value,parse_def_date_value,parse_def_time_value))(input)?;
    let (input,_)=space1(input)?;
    Ok((input,cv))
}

fn parse_def_text_value(input: &str) -> IResult<&str, CellValue> {
    let (input,_)=alt((tag("text"),tag("t")))(input)?;
    let def= CellValue::Text(String::new());
    Ok((input,def))
}

fn parse_def_integer_value(input: &str) -> IResult<&str, CellValue> {
    let (input,_)=alt((tag("int"),tag("i")))(input)?;
    let def=CellValue::Integer(0);
    Ok((input,def))
}

fn parse_def_float_value(input: &str) -> IResult<&str, CellValue> {
    let (input,_)=alt((tag("float"),tag("f")))(input)?;
    let def=CellValue::Float(0.0);
    Ok((input,def))
}

fn parse_def_bool_value(input: &str) -> IResult<&str, CellValue> {
    let (input,_)=alt((tag("bool"),tag("b")))(input)?;
    let def=CellValue::Boolean(false);
    Ok((input,def))
}

fn parse_def_date_value(input: &str) -> IResult<&str, CellValue> {
    let (input,_)=alt((tag("date"),tag("d")))(input)?;
    let def = CellValue::Date(Local::now());
    Ok((input,def))
}

fn parse_def_time_value(input: &str) -> IResult<&str, CellValue> {
    let (input,_)=tag("time")(input)?;
    let def = CellValue::TimeStamp(Local::now());
    Ok((input,def))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_command(){
        assert_eq!(Ok(("",Command::Quit)), parse_command("q"));
        assert_eq!(Ok(("",Command::Quit)), parse_command("quit"));
        assert_eq!(Ok(("",Command::Choose(CellID{row:0,col:0}))), parse_command("c A1"));
        assert_eq!(Ok(("",Command::Choose(CellID{row:0,col:0}))), parse_command("choose A1"));
        assert_eq!(Ok(("",Command::SetValue(CellValue::Text("hello".to_string())))), parse_command("t hello"));
        assert_eq!(Ok(("",Command::SetValue(CellValue::Text("hello".to_string())))), parse_command("text hello"));
        assert_eq!(Ok(("",Command::SetValue(CellValue::Text(String::new())))), parse_command("text"));
        assert_eq!(Ok(("",Command::SetValue(CellValue::Integer(1)))), parse_command("i 1"));
        assert_eq!(Ok(("",Command::SetValue(CellValue::Integer(123)))), parse_command("i 123"));
        assert_eq!(Ok(("",Command::SetValue(CellValue::Integer(0)))), parse_command("i"));
        assert_eq!(Ok(("",Command::Save(None))), parse_command("s"));
        assert_eq!(Ok(("",Command::Save(None))), parse_command("save"));

        assert_eq!(Ok(("",Command::Load("test1.tablr".to_string()))), parse_command("l test1.tablr"));
        assert_eq!(Ok(("",Command::Load("test1.tablr".to_string()))), parse_command("load test1.tablr"));

        assert_eq!(Ok(("",Command::SaveCSV(None))), parse_command("sc"));
        assert_eq!(Ok(("",Command::SaveCSV(None))), parse_command("savecsv"));
        assert_eq!(Ok(("",Command::SaveCSV(Some("test1.csv".to_string())))), parse_command("sc test1.csv"));
        assert_eq!(Ok(("",Command::SaveCSV(Some("test1.csv".to_string())))), parse_command("savecsv test1.csv"));

        let id=InputDescription{headers:1, template:vec![CellValue::Text(String::new()), CellValue::Integer(0)]};
        assert_eq!(Ok(("",Command::LoadCSV("test1.csv".to_string(),id.clone()))), parse_command("lc 1 t i test1.csv"));
        assert_eq!(Ok(("",Command::LoadCSV("test1.csv".to_string(),id))), parse_command("loadcsv 1 text int test1.csv"));

        let ptime = parse_command("time");
        assert!(ptime.is_ok());
        if let Ok((_,cmd)) = ptime {
            match cmd {
                Command::SetValue(v) => {
                    match v {
                        CellValue::TimeStamp(_) =>(),
                        _ => panic!("Not a timestamp"),
                    }
                }
                _ => panic!("Not a set value"),
            }
        }
        let pdate = parse_command("date");
        assert!(pdate.is_ok());
        if let Ok((_,cmd)) = pdate {
            match cmd {
                Command::SetValue(v) => {
                    match v {
                        CellValue::Date(_) =>(),
                        _ => panic!("Not a date"),
                    }
                }
                _ => panic!("Not a set value"),
            }
        }
    }
}