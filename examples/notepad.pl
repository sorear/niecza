# notepad.pl for Niecza Perl 6
# needs libmono-winforms2.0-cil on Debian and Ubuntu

# CLR library dependencies - use either Mono or .NET
constant $DRAWING = "System.Drawing, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a";
constant $FORMS   = "System.Windows.Forms, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089";
constant Font     = CLR::("System.Drawing.Font,$DRAWING");
constant MenuItem = CLR::("System.Windows.Forms.MenuItem,$FORMS");
constant Shortcut = CLR::("System.Windows.Forms.Shortcut,$FORMS");

# Application variables
my $filename;
my $modified = False;

# The TextBox does all the interactive text editing
my $textbox = CLR::("System.Windows.Forms.TextBox,$FORMS").new;
$textbox.Font = Font.new("Mono", 10);
$textbox.Dock = CLR::("System.Windows.Forms.DockStyle,$FORMS").Fill;
$textbox.Multiline  = True;
$textbox.WordWrap   = False;
$textbox.AcceptsTab = True;
if @*ARGS {  # Allow passing a file name from the command line
    $filename = @*ARGS[0];
    $textbox.AppendText(slurp $filename);  # TODO: replace with the following:
#   $textbox.Text = slurp $filename;
#   $textbox.Modified = False;
#   $textbox.DeselectAll;
    # TODO: but these leave all the text *selected* :P
}
$textbox.add_TextChanged(&TextChanged);
$textbox.ScrollBars = CLR::("System.Windows.Forms.ScrollBars,$FORMS").Both;

# Define Menu and MenuItems
my $menuFile = MenuItem.new("&File");
$menuFile.MenuItems.Add(MenuItem.new("&New",    &New,    Shortcut.CtrlN));
$menuFile.MenuItems.Add(MenuItem.new("&Open",   &Open,   Shortcut.CtrlO));
$menuFile.MenuItems.Add(MenuItem.new("&Save",   &Save,   Shortcut.CtrlS));
$menuFile.MenuItems.Add(MenuItem.new("Save&As", &SaveAs, Shortcut.CtrlA));
$menuFile.MenuItems.Add(MenuItem.new("E&xit",   &Exit,   Shortcut.CtrlQ));
my $menu = CLR::("System.Windows.Forms.MainMenu,$FORMS").new;
my $menuEdit = MenuItem.new("&Edit");
$menuEdit.MenuItems.Add(MenuItem.new("&Undo",   &Undo,   Shortcut.CtrlZ));
$menuEdit.MenuItems.Add(MenuItem.new("Cu&t",    &Cut,    Shortcut.CtrlX));
$menuEdit.MenuItems.Add(MenuItem.new("&Copy",   &Copy,   Shortcut.CtrlC));
$menuEdit.MenuItems.Add(MenuItem.new("&Paste",  &Paste,  Shortcut.CtrlV));
$menu.MenuItems.Add($menuFile);
$menu.MenuItems.Add($menuEdit);

# The form is the main application window
my $form = CLR::("System.Windows.Forms.Form,$FORMS").new;
$form.Size = CLR::("System.Drawing.Size,$DRAWING").new(600,400);
$form.Menu = $menu;
$form.Controls.Add($textbox);
setformtitle();
CLR::("System.Windows.Forms.Application,$FORMS").Run($form);

# Event handlers
sub New($sender, $eventargs) #OK not used
{
    $textbox.Clear;
    $filename = Nil;
    $modified = False;
    setformtitle();
}

sub Open($sender, $eventargs) #OK not used
{
    my $dialogOpen  = CLR::("System.Windows.Forms.OpenFileDialog,$FORMS").new;
    $dialogOpen.Filter = "Text Files (*.txt)|*.txt|All Files (*.*)|*.*";
    $dialogOpen.FilterIndex = 1;
    if ~ $dialogOpen.ShowDialog eq 'OK' {
        $filename = $dialogOpen.FileName;
        $textbox.Text = slurp $filename;
        $modified = False;
        setformtitle();
    }
}

sub Save($sender, $eventargs) #OK not used
{
    if defined $filename { spurt $filename, $textbox.Text; $modified=False; setformtitle(); }
    else                 { SaveAs($sender, $eventargs); }
}

sub SaveAs($sender, $eventargs) #OK not used
{
    my $dialogSave  = CLR::("System.Windows.Forms.SaveFileDialog,$FORMS").new;
    $dialogSave.Filter = "Text Files (*.txt)|*.txt|All Files (*.*)|*.*";
    $dialogSave.FilterIndex = 1;
    if ~ $dialogSave.ShowDialog eq 'OK' {
        $filename = $dialogSave.FileName;
        spurt $filename, $textbox.Text;
        $modified = False;
        setformtitle();
    }
}

sub Exit($sender, $eventargs) #OK not used
{
    if $modified {
        # TODO: Offer to save edited text
    }
    CLR::("System.Windows.Forms.Application,$FORMS").Exit;
}

sub Undo($sender, $eventargs) #OK not used
{
    $textbox.Undo; $textbox.Refresh;
}

sub Cut($sender, $eventargs) #OK not used
{
    if $textbox.SelectionLength > 0 {
        $textbox.Cut;
    }
}

sub Copy($sender, $eventargs) #OK not used
{
    if $textbox.SelectionLength > 0 {
        $textbox.Copy;
    }
}

sub Paste($sender, $eventargs) #OK not used
{
    $textbox.Paste;
}

sub TextChanged($sender, $eventargs) #OK not used
{
    $modified = True;
    setformtitle();
}

# helper subroutines
sub setformtitle()
{
    my $name = $filename // 'untitled'; # /
    $form.Text = ($modified ?? '*' !! '') ~ $name ~ ' - Niecza Notepad';
}
