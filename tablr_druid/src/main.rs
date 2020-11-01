use druid::widget::{Flex, Label, Painter, TextBox, List, Scroll};
use druid::{theme, AppLauncher,Color, Data, Lens, LocalizedString, PlatformError, RenderContext, Widget, WidgetExt, WindowDesc};

use tablr_lib::*;

fn main() -> Result<(), PlatformError> {
    let main_window = WindowDesc::new(ui_builder).title("Tablr");
    let data = WorkbookState{workbook:Workbook::new(), active:None};
    AppLauncher::with_window(main_window)
        .use_simple_logger()
        .launch(data)
}

#[derive(Clone, Data, Lens)]
struct WorkbookState {
    #[data(same_fn = "PartialEq::eq")]
    workbook: Workbook,
    #[data(same_fn = "PartialEq::eq")]
    active: Option<CellID>,
}

/*fn ui_builder() -> impl Widget<u32> {
    // The label text will be computed dynamically based on the current locale and count
    let text =
        LocalizedString::new("hello-counter").with_arg("count", |data: &u32, _env| (*data).into());
    let label = Label::new(text).padding(5.0).center();
    let button = Button::new("increment")
        .on_click(|_ctx, data, _env| *data += 1)
        .padding(5.0);

    Flex::column().with_child(label).with_child(button)
}*/

fn ui_builder() -> impl Widget<WorkbookState> {
    let size= 10;
    let mut f = Flex::column().with_child(col_headers(size));
    for r in 0..10 {
        f=f.with_spacer(1.0).with_child(data_row(r,size));
    }
    f.with_spacer(1.0).with_child(command_box())
}

fn col_headers(size: usize) -> impl Widget<WorkbookState> {
    let mut r = Flex::row().with_flex_child(coord_label(String::new()), 1.0)
        .with_spacer(1.0);
    for i in 0..size {
        r=r.with_flex_child(coord_label(column_name(i)), 1.0)
            .with_spacer(1.0);
    }
    r
}

fn data_row(row: u128, size: usize)  -> impl Widget<WorkbookState> {
    let mut r = Flex::row().with_flex_child(coord_label(format!("{}",row+1)), 1.0)
        .with_spacer(1.0);
    for col in 0..size {
        r=r.with_flex_child(data_label(CellID{row,col}), 1.0)
            .with_spacer(1.0);
    }
    r
}

fn command_box()  -> impl Widget<WorkbookState> {
    Flex::row().with_flex_child(Label::new("your command goes here"),10.0)
}

fn coord_label(label: String) -> impl Widget<WorkbookState> {
    let painter = Painter::new(|ctx, _, env| {
        let bounds = ctx.size().to_rect();

        ctx.fill(bounds, &env.get(theme::PRIMARY_DARK));

        if ctx.is_hot() {
            ctx.stroke(bounds.inset(-0.5), &Color::WHITE, 1.0);
        }

        if ctx.is_active() {
            ctx.fill(bounds, &env.get(theme::PRIMARY_LIGHT));
        }
    });

    Label::new(label)
        .with_text_size(24.)
        .center()
        .background(painter)
}


fn data_label(id: CellID) -> impl Widget<WorkbookState> {
    let painter = Painter::new(|ctx, _, env| {
        let bounds = ctx.size().to_rect();

        ctx.fill(bounds, &env.get(theme::BACKGROUND_LIGHT));

        if ctx.is_hot() {
            ctx.stroke(bounds.inset(-0.5), &Color::WHITE, 1.0);
        }

        if ctx.is_active() {
            ctx.fill(bounds, &Color::rgb8(0x71, 0x71, 0x71));
        }
    });

    Label::new(String::new())
        .with_text_size(24.)
        .center()
        .background(painter)
        .on_click(move |ctx, data: &mut WorkbookState, _env| {
            ctx.set_active(true);
            data.active=Some(id);
        })
}

