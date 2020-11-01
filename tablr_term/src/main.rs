use tablr_lib::*;
use prettytable::{Table, Row, Cell};
use std::io::{stdout,stdin,Write};

mod command;
use command::*;

fn main() {
    println!("Welcome to Tablr");
    let mut state=TablrState::new();
    print_table(&state);
    print_prompt(&state);
    loop {
        match read() {
            Some(Command::Quit)=>break,
            Some(cmd)=>{
                eval(&mut state, cmd);
                print_prompt(&state);
            },
            None=> print_prompt(&state),
        }
    }
    
}

fn read() -> Option<Command> {
    let mut buffer = String::new();
    match stdin().read_line(&mut buffer){
        Ok(_sz)=> {
            let s =buffer.trim_end_matches("\r\n");
            if s.is_empty() {
                None
            } else {
                match parse_command(s) {
                    Ok((_i,cmd))=>Some(cmd),
                    Err(err) => {println!("{}",err); None},
                }
            }
        },
        Err(err) => {
            println!("{}",err); 
            None
        },
    }
    
}

fn eval(state: &mut TablrState, c:Command){
    match c {
        Command::Quit=>(),
        Command::Load(path)=>(),
        Command::Save(path)=>(),
        Command::Help=>println!("{}",HELP),
        Command::Error(err)=>{
            println!("Error parsing value: {}",err);
        },
        Command::Choose(id)=>{
            state.current_cell=id;
        },
        Command::SetValue(cv)=> {
            let r=state.runtime.set_value(state.current_sheet, state.current_cell,cv);
            print_table(&state);
            print_errors(r);
        },
        Command::SetFormula(f)=> {
            if f.is_empty(){
                let ef=state.runtime.workbook.sheets[state.current_sheet].get_cell(&state.current_cell).map(|c| &c.formula);
                match ef {
                    Some(Some(s))=>println!("{}",s),
                    _ => println!("No formula"),
                };
            } else {
                let r=state.runtime.set_formula_str(state.current_sheet, state.current_cell, &f);
                print_table(&state);
                print_errors(r);
            }
        },
    }
}

const HELP: &'static str = "q to quit
l | load <path> to load a saved file
s | save <path?> to save to a path (save to previous path if not provided)
c | choose <cellid> to choose a cell
t | text <value?> to set a text value to a cell
i | int <value?> to set an integer value to a cell
f | float <value?> to set an float value to a cell
b | bool <value?> to set a boolean value to a cell
d | date <value?> to set a date value to a cell
time <value?> to set a timestamp value to a cell
f | formula <value?> to set a formula for a cell, emtpy value to display the current formula";

fn print_prompt(state: &TablrState){
    print!("{}> ",state.current_cell);
    let _=stdout().flush();
}

fn print_errors(r: Result<Vec<(CellID,Result<CellValue,EvalError>)>,EvalError>){

}

struct TablrState {
    runtime: Runtime,
    path: Option<String>,
    current_sheet: usize,
    current_cell: CellID,
}

impl TablrState {
    pub fn new() -> Self {
        TablrState {
            runtime: Runtime::new(),
            path: None,
            current_sheet: 0,
            current_cell: CellID{row:0,col:0},
        }
    }
}

fn print_table(state: &TablrState) {
    let sheet = &state.runtime.workbook.sheets[state.current_sheet];
    let mut table = Table::new();
    let (mut max_col,mut max_row)= sheet.metadata.size();
    max_col=max_col.max(10);
    max_row=max_row.max(10);
    
    let mut hs=vec![];
    hs.push(Cell::new(""));
    for c in 0..max_col {
        hs.push(Cell::new(&column_name(c)));
    }
    table.add_row(Row::new(hs));

    for r in 0..max_row {
        let mut row=vec![];
        row.push(Cell::new(&format!("{}",r+1)));
        for c in 0..max_col {
            let cv=sheet.get_cell(&CellID{row:r,col:c}).map(|c| &c.value).unwrap_or(&CellValue::Empty);
            row.push(Cell::new(&cv.to_string()));
        }
        table.add_row(Row::new(row));
    }
    table.printstd();

}