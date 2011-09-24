# Main documentation: http://docs.go-mono.com, particularly
# Gnome (for Gdk and Gtk) and Mono (for Cairo) libraries.
# See also: The X-Windows Disaster at http://www.art.net/~hopkins/Don/unix-haters/handbook.html

constant $GTK  = "gtk-sharp,  Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
constant $GDK  = "gdk-sharp,  Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
constant $GLIB = "glib-sharp, Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
# use 'gacutil -l' to look up similar module details

constant Application    = CLR::("Gtk.Application,$GTK");
constant Window         = CLR::("Gtk.Window,$GTK");
constant GdkCairoHelper = CLR::("Gdk.CairoHelper,$GDK");
constant GtkDrawingArea = CLR::("Gtk.DrawingArea,$GTK");
constant GLibTimeout    = CLR::("GLib.Timeout,$GLIB");

Application.Init;
my $window = Window.new("sierpinski");
my $windowSizeX = 640; my $windowSizeY = 560;
$window.Resize($windowSizeX, $windowSizeY);
my $drawingarea = GtkDrawingArea.new;
$drawingarea.add_ExposeEvent: sub ($obj, $args) {
    $args;  # suppress 'declared but not used' warnings
    my $cc = GdkCairoHelper.Create($obj.GdkWindow);  # Cairo Context
    # TODO: the following two lines pass parameters by value, need to pass by references to integers
    # my $windowX; my $windowY; my $windowWidth; my $windowHeight; my $windowDepth;
    # $obj.GdkWindow.GetGeometry($windowX, $windowY, $windowWidth, $windowHeight, $windowDepth);
    # Tracked as https://github.com/sorear/niecza/issues/57
    # TODO: remove the following one line cheat that works around the above problem
    my $windowWidth = $windowSizeX; my $windowHeight = $windowSizeY;
    $cc.SetSourceRGB(0.6.Num, 1, 0.6.Num); $cc.Paint();  # light green background
    # Start the recursive drawing process
    my $x0=0; my $y0=0; my  $x1=$windowWidth-1; my $y1=$windowHeight/2;
    my $x2=0; my $y2=$windowHeight-1;
    my $depth = Sierpinski($cc, $x0, $y0, $x1, $y1, $x2, $y2, True, 1);
    my $label = sprintf("%d x %d, %d levels", $windowWidth, $windowHeight, $depth);
    say $label;
    $cc.Target.Dispose;
    $cc.dispose-hack; # Should be $cc.Dispose but CLR interop cannot call that
    # Tracked as https://github.com/sorear/niecza/issues/56
};
$window.add_DeleteEvent: sub ($obj, $args) {
    $obj; $args; # suppress "declared but not used" Potential difficulties
    Application.Quit;
};
$window.Add($drawingarea);
$window.ShowAll;
Application.Run;

sub Sierpinski($cc, $x0, $y0, $x1, $y1, $x2, $y2, $colorflag, $depth is copy)
{
    if $colorflag { $cc.SetSourceRGB(0.6.Num, 0, 0.8.Num); }  # indigo
    else          { $cc.SetSourceRGB(1, 1, 0.8.Num); }  # light yellow
    # First draw the entire main triangle in one color
    $cc.MoveTo($x0.Int, $y0.Int); $cc.LineTo($x1.Int, $y1.Int);
    $cc.LineTo($x2.Int, $y2.Int); $cc.LineTo($x0.Int, $y0.Int);
    $cc.Fill;
    $cc.Stroke;
    if (($x1-$x0)>2) && (($y2-$y0)>2) {  # Prepare to recurse
        ++$depth;
        # Calculate the midpoints of the 3 edges of the triangle
        my $x01=(($x0+$x1)/2).Int; my $y01=(($y0+$y1)/2).Int;
        my $x12=(($x1+$x2)/2).Int; my $y12=(($y1+$y2)/2).Int;
        my $x20=(($x2+$x0)/2).Int; my $y20=(($y2+$y0)/2).Int;
        # Recursively draw three smaller triangles
        Sierpinski($cc, $x0, $y0,  $x01,$y01, $x20,$y20, !$colorflag, $depth);
        Sierpinski($cc, $x01,$y01, $x1, $y1,  $x12,$y12, !$colorflag, $depth);
        $depth =
        Sierpinski($cc, $x20,$y20, $x12,$y12, $x2, $y2,  !$colorflag, $depth);
    }
    $depth;
}
