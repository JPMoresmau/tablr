use tablr_lib::*;
use prettytable::{Table, Row, Cell,Attr, color};
use std::collections::{HashSet};
use rustyline::error::ReadlineError;
use rustyline::Editor;

mod command;
use command::*;

fn main() {
    println!("Welcome to Tablr");
    let mut state=TablrState::new();
    print_table(&state, None);
    let mut rl = Editor::<()>::new();
    //print_prompt(&state);
    loop {
       
        match read(&state, &mut rl) {
            Some(Command::Quit)=>break,
            Some(cmd)=> eval(&mut state, cmd),
            None=> (),
        }
    }
    
}

fn read(state: &TablrState,rl : &mut Editor<()>) -> Option<Command> {
    let readline = rl.readline(&format!("{}>> ",state.current_cell));
    match readline{
        Ok(line)=> {
            if line.is_empty() {
                None
            } else {
                rl.add_history_entry(line.as_str());
                match parse_command(&line) {
                    Ok((_i,cmd))=>Some(cmd),
                    Err(err) => {println!("{}",err); None},
                }
            }
        },
        Err(ReadlineError::Interrupted) => {
            Some(Command::Quit)
        },
        Err(ReadlineError::Eof) => {
            Some(Command::Quit)
        },
        Err(err) => {
            println!("{}",err); 
            None
        },
    }
    
}

fn with_extension(mut path: String) -> String {
    if !path.ends_with(".tablr"){
        path.push_str(".tablr");
    }
    path
}

fn with_csv_extension(mut path: String) -> String {
    if !path.ends_with(".tablr"){
        path=path.trim_end_matches(".tablr").to_string();
    }
    if !path.ends_with(".csv"){
        path.push_str(".csv");
    }
    path
}

fn eval(state: &mut TablrState, c:Command){
    match c {
        Command::Quit=>(),
        Command::Load(path)=>{
            let path =with_extension(path);
            state.path=Some(path.clone());
            match load(&path) {
                Ok(wk)=>{
                    let vr=state.runtime.load(wk);
                    print_table(&state, None);
                    vr.iter().for_each(|r| print_errors(r));
                },
                Err(err)=> println!("Load error: {}",err),
            }
        },
        Command::LoadCSV(path, desc)=>{
            let path =with_csv_extension(path);
            match load_csv(&path, &desc) {
                Ok(sh)=>{
                    let mut wk=Workbook::new();
                    wk.sheets[0]=sh;
                    let vr=state.runtime.load(wk);
                    print_table(&state, None);
                    vr.iter().for_each(|r| print_errors(r));
                },
                Err(err)=> println!("Load error: {}",err),
            }
        },
        Command::Save(path)=>{
            let npath = path.map(with_extension).or(state.path.take());
            match npath {
                None=>println!("No path provided"),
                Some(p)=> {
                    state.path=Some(p.clone());
                    match save(&state.runtime.workbook, &p){
                        Ok(_)=> println!("Saved to {}",p),
                        Err(err)=> println!("Save error: {}",err),
                    }
                },
            }
        },
        Command::SaveCSV(path)=>{
            let npath = path.or(state.path.clone()).map(with_csv_extension);
            match npath {
                None=>println!("No path provided"),
                Some(p)=> {
                    state.path=Some(p.clone());
                    match save_csv(&state.runtime.workbook.sheets[state.current_sheet], &p){
                        Ok(_)=> println!("Saved to {}",p),
                        Err(err)=> println!("Save error: {}",err),
                    }
                },
            }
        },
        Command::Help=>println!("{}",HELP),
        Command::Error(err)=>{
            println!("Error parsing value: {}",err);
        },
        Command::Choose(id)=>{
            state.current_cell=id;
        },
        Command::SetValue(cv)=> {
            let r=state.runtime.set_value(state.current_sheet, state.current_cell,cv);
            if r.is_ok(){
                state.current_cell=state.current_cell.next_row();
            }
            print_table(&state, Some(&r));
            print_errors(&r);
        },
        Command::SetFormula(f)=> {
            if f.is_empty(){
                let ef=state.runtime.workbook.sheets[state.current_sheet].cells.get_cell_extra(&state.current_cell);
                match ef {
                    Some(s)=>println!("{}",s),
                    _ => println!("No formula"),
                };
            } else {
                let r=state.runtime.set_formula_str(state.current_sheet, state.current_cell, &f);
                if r.is_ok(){
                    state.current_cell=state.current_cell.next_row();
                }
                print_table(&state, Some(&r));
                print_errors(&r);
            }
        },
        Command::Copy(from)=> {
            let r=state.runtime.copy_cell(state.current_sheet, from,state.current_cell);
            if r.is_ok(){
                state.current_cell=state.current_cell.next_row();
            }
            print_table(&state, Some(&r));
            print_errors(&r);
        }
    }
}

const HELP: &'static str = "q to quit
l | load <path> to load a saved file
s | save <path?> to save to a path (save to previous path if not provided)
lc | loadcsv <headerCount> <column types> <path> to load a CSV file: lc 1 t i example.csv loads a file with one row of headers, first column is text, second column is integer
sc | savecsv <path?> to save to a CSV file
c | choose <cellid> to choose a cell: c A2
cp | copy <cellid> to copy value and formula from cellid to current cell (formula gets translated)
t | text <value?> to set a text value to a cell
i | int <value?> to set an integer value to a cell
f | float <value?> to set an float value to a cell
b | bool <value?> to set a boolean value to a cell
d | date <value?> to set a date value to a cell
time <value?> to set a timestamp value to a cell
f | formula <value?> to set a formula for a cell, emtpy value to display the current formula";

//fn print_prompt(state: &TablrState){
//    print!("{}> ",state.current_cell);
//    let _=stdout().flush();
//}

fn print_errors(r: &SetResult){
    match &r.0 {
        Err(err) => println!("{}",err),
        Ok(v) => v.iter().for_each(|(c,rc)|{
            match rc {
                Err(err) => println!("{}: {}",c,err),
                Ok(_cv)=>(),
            }
        }),
    }
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

fn print_table(state: &TablrState,or: Option<&SetResult>) {
    let sheet = &state.runtime.workbook.sheets[state.current_sheet];
    let mut table = Table::new();
    let (mut max_col,mut max_row)= sheet.cells.metadata.size();
    max_col=max_col.max(10);
    max_row=max_row.max(10);
    
    let mut hs=vec![];
    hs.push(Cell::new(""));
    for c in 0..max_col {
        hs.push(Cell::new(&column_name(c)));
    }
    table.add_row(Row::new(hs));

    let (oks, errs) = or.map(|r| r.impacted_cells()).unwrap_or((HashSet::new(),HashSet::new()));

    for r in 0..max_row {
        let mut row=vec![];
        row.push(Cell::new(&format!("{}",r+1)));
        for c in 0..max_col {
            let cid = CellID{row:r,col:c};
            let cv=sheet.cells.get_cell_value(&cid).unwrap_or(&CellValue::Empty);
            let mut cell = Cell::new(&cv.to_string());
            if oks.contains(&cid){
                cell=cell.with_style(Attr::BackgroundColor(color::GREEN))
                    .with_style(Attr::ForegroundColor(color::BLACK));
            } else if errs.contains(&cid){
                cell=cell.with_style(Attr::BackgroundColor(color::RED))
                .with_style(Attr::ForegroundColor(color::BLACK));
            }
            row.push(cell);
        }
        table.add_row(Row::new(row));
    }
    table.printstd();

}