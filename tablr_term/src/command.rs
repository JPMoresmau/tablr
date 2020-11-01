use tablr_lib::*;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag},
    character::complete::{space1, space0},
    combinator::{eof},
  };
use chrono::{Local,DateTime};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Command {
    Load(String),
    Save(Option<String>),
    Quit,
    Help,
    Choose(CellID),
    SetValue(CellValue),
    SetFormula(String),
    Error(String),
}

pub fn parse_command(input: &str) -> IResult<&str, Command> {
    alt((parse_load,parse_save,parse_quit,parse_help,parse_formula,parse_choose,parse_value))(input)
}

fn parse_load(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("l"),tag("load")))(input)?;
    Ok(("",Command::Load(input.to_string())))
}

fn parse_save(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("s"),tag("save")))(input)?;
    let (input,_)=space1(input)?;
    let s = input.to_string();
    let op = if s.is_empty() {
        None
    } else {
        Some(s)
    };
    Ok(("",Command::Save(op)))
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
    let (input,_)=alt((tag("text"),tag("t")))(input)?;
    let def= CellValue::Text(String::new());
    parse_value_with_default(input, def)
}

fn parse_integer_value(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("int"),tag("i")))(input)?;
    let def=CellValue::Integer(0);
    parse_value_with_default(input, def)
}

fn parse_float_value(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("float"),tag("f")))(input)?;
    let def=CellValue::Float(0.0);
    parse_value_with_default(input, def)
}

fn parse_bool_value(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("bool"),tag("b")))(input)?;
    let def=CellValue::Boolean(false);
    parse_value_with_default(input, def)
    
}

fn parse_date_value(input: &str) -> IResult<&str, Command> {
    let (input,_)=alt((tag("date"),tag("d")))(input)?;
    let def = CellValue::Date(DateTime::from(Local::now()));
    parse_value_with_default(input, def)
}

fn parse_time_value(input: &str) -> IResult<&str, Command> {
    let (input,_)=tag("time")(input)?;
    let def = CellValue::TimeStamp(DateTime::from(Local::now()));
    parse_value_with_default(input, def)
}


fn parse_value_with_default(input: &str,def: CellValue) -> IResult<&str, Command> {
    let (input,_)=space0(input)?;
    if input.is_empty() {
        Ok(("",Command::SetValue(def)))
    } else {
        Ok(("",match def.parse_similar(input){
            Ok(cv) => Command::SetValue(cv),
            Err(err) => Command::Error(format!("{}",err)),
        }))
    }
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
    }
}