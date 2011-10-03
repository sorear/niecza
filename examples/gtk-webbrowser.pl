# gtk-webbrowser.pl - based on the Gtk C# example
# see Gnome Libraries, Gtk, HTML class at http://docs.go-mono.com/
# needs libgtkhtml3.16-cil on Debian or Ubuntu

# Names that occur multiple times.  Names used only once appear inline.
constant $GTK     = "gtk-sharp,     Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
constant $GTKHTML = "gtkhtml-sharp, Version=3.16.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
constant $SYSTEM  = "System,        Version=2.0.0.0,  Culture=neutral, PublicKeyToken=b77a5c561934e089";
constant Application  = CLR::("Gtk.Application,$GTK");

# ---------------------------- Main program ----------------------------
Application.Init;
my $currentUrl; my @historyUrl; my $name = "Niecza web browser";
my $label = CLR::("Gtk.Label,$GTK").new("Address:");
my $entry = CLR::("Gtk.Entry,$GTK").new;
$entry.add_Activated(&EntryActivated);
my $buttonGo = CLR::("Gtk.Button,$GTK").new("Go!");
$buttonGo.add_Clicked(&EntryActivated);  # share $entry's handler
my $buttonBack = CLR::("Gtk.Button,$GTK").new("Back");
$buttonBack.add_Clicked(&BackClicked);
my $hbox = CLR::("Gtk.HBox,$GTK").new(False, 1);
$hbox.PackStart($label, False, False, 1);
$hbox.PackStart($entry, True, True, 1);
$hbox.PackStart($buttonGo, False, False, 1);
$hbox.PackStart($buttonBack, False, False, 1);
my $html = CLR::("Gtk.HTML,$GTKHTML").new;
$html.add_LinkClicked(&LinkClicked);
my $sw = CLR::("Gtk.ScrolledWindow,$GTK").new;
$sw.VscrollbarPolicy = CLR::("Gtk.PolicyType,$GTK").Always;
$sw.HscrollbarPolicy = CLR::("Gtk.PolicyType,$GTK").Always;
$sw.Add($html);
my $vbox = CLR::("Gtk.VBox,$GTK").new(False, 1);
$vbox.PackStart($hbox, False, False, 1);
$vbox.PackStart($sw, True, True, 1);
my $win = CLR::("Gtk.Window,$GTK").new($name);
$win.SetDefaultSize(800, 600);
$win.add_DeleteEvent(sub ($o,$a) { Application.Quit(); });  #OK not used
$win.Add($vbox);
$win.ShowAll;
Application.Run;

# --------------------------- Event handlers ---------------------------
sub EntryActivated($obj, $args)  #OK not used
{
    if $currentUrl { push @historyUrl, $currentUrl; }
    $currentUrl = $entry.Text.subst(/^\s+/,"").subst(/\s+$/,"");  # trim
    LoadHtml($currentUrl);
}

sub LinkClicked($obj, $args)  #OK not used
{
    my $newUrl =  $args.Url ~~ /^ 'http://' /  # decide absolute or relative '
        ?? $args.Url
        !! $currentUrl ~ $args.Url;
    try {
        LoadHtml($newUrl);      # apparently throws numerous exceptions
        CATCH { }               # hide the evidence of the failure
        push @historyUrl, $currentUrl;
        $currentUrl = $newUrl;
    }
}

sub BackClicked($obj, $args) #OK not used
{
    if @historyUrl { LoadHtml($currentUrl = pop @historyUrl); }
}

# ------------------------- Helper subroutines -------------------------
sub LoadHtml($Url is copy)
{
    unless $Url ~~ /^ [http|https] '://' / { $Url = 'http://' ~ $Url } # '
    my $web_request = CLR::("System.Net.WebRequest,$SYSTEM").Create($Url);
    my $web_response = $web_request.GetResponse;
    my $stream = $web_response.GetResponseStream;
    my $buffer = CLR::("System.Byte[]").new(8192);
    my $html_stream = $html.Begin;
    while (my $count = $stream.Read($buffer, 0, 8192)) != 0 {
        $html_stream.Write($buffer, $count);
    }
    $html.End($html_stream, CLR::("Gtk.HTMLStreamStatus,$GTKHTML").Ok);
    $entry.Text = $Url;
    $win.Title = $html.Title ~ " - $name";
}
