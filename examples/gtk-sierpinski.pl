# Main documentation: http://docs.go-mono.com, particularly
# Gnome (for Gdk and Gtk) and Mono (for Cairo) libraries.
# See also: The X-Windows Disaster at http://www.art.net/~hopkins/Don/unix-haters/handbook.html

constant $GTK  = "gtk-sharp, Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
constant $GDK  = "gdk-sharp, Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
# use 'gacutil -l' to look up similar module details

constant Application    = CLR::("Gtk.Application,$GTK");
constant Window         = CLR::("Gtk.Window,$GTK");
constant GdkCairoHelper = CLR::("Gdk.CairoHelper,$GDK");
constant GtkDrawingArea = CLR::("Gtk.DrawingArea,$GTK");

Application.Init;
my $window = Window.new("sierpinski");
my $windowSizeX = 640; my $windowSizeY = 560;
$window.Resize($windowSizeX, $windowSizeY);  # TODO: resize at runtime NYI
my $drawingarea = GtkDrawingArea.new;
$drawingarea.add_ExposeEvent(&ExposeEvent);
$window.add_DeleteEvent(&DeleteEvent);
$window.Add($drawingarea);
$window.ShowAll;
Application.Run;  # end of main program, it's all over when this returns

sub DeleteEvent($obj, $args) {  #OK not used
    Application.Quit;
};

sub ExposeEvent($obj, $args)
{
    $args;  # suppress "declared but not used" "Potential difficulties"
    my $cc = GdkCairoHelper.Create($obj.GdkWindow);  # Cairo Context
    my $windowX=0; my $windowY=0; my $windowWidth=0; my $windowHeight=0; my $windowDepth=0;
    $obj.GdkWindow.GetGeometry($windowX, $windowY, $windowWidth, $windowHeight, $windowDepth);
    $cc.SetSourceRGB(0.6, 1, 0.6); $cc.Paint;  # pale green background
    # Start the recursive drawing process
    my $x0=0; my $y0=0; my  $x1=$windowWidth-1; my $y1=$windowHeight/2;
    my $x2=0; my $y2=$windowHeight-1;
    my $depth = Sierpinski($cc, $x0, $y0, $x1, $y1, $x2, $y2, True, 1);
    my $text = sprintf("%d x %d, %d levels", $windowWidth, $windowHeight, $depth);
    $cc.SetSourceRGB(0.6, 0.6, 1);  # pale blue
    $cc.SetFontSize($windowWidth * 0.07);
    my $textWidth  = $cc.TextExtents($text).Width;
    my $textHeight = $cc.TextExtents($text).Height;
    $cc.MoveTo($windowWidth*0.98 - $textWidth, $windowHeight*0.01 + $textHeight);
    $cc.ShowText($text);
    $cc.Target.Dispose;
    $cc.dispose-hack; # Should be $cc.Dispose but CLR interop cannot call that
    # Tracked as https://github.com/sorear/niecza/issues/56
};

sub Sierpinski($cc, $x0, $y0, $x1, $y1, $x2, $y2, $colorflag, $depth is copy)
{
    if $colorflag { $cc.SetSourceRGB(0.6, 0, 0.8); }  # indigo
    else          { $cc.SetSourceRGB(  1, 1, 0.8); }  # pale yellow
    # First draw the entire main triangle in the one color
    $cc.MoveTo($x0, $y0); $cc.LineTo($x1, $y1);
    $cc.LineTo($x2, $y2); $cc.LineTo($x0, $y0);
    $cc.Fill;
    $cc.Stroke;
    if (($x1-$x0)>2) && (($y2-$y0)>2) {  # Prepare to recurse
        ++$depth;
        # Calculate the midpoints of the 3 edges of the triangle
        # note - these .Int conversions make a very big speed difference
        my $x01=(($x0+$x1)/2).Int; my $y01=(($y0+$y1)/2).Int;
        my $x12=(($x1+$x2)/2).Int; my $y12=(($y1+$y2)/2).Int;
        my $x20=(($x2+$x0)/2).Int; my $y20=(($y2+$y0)/2).Int;
        # Recursively draw three smaller triangles in the other color
        Sierpinski($cc, $x0, $y0,  $x01,$y01, $x20,$y20, !$colorflag, $depth);
        Sierpinski($cc, $x01,$y01, $x1, $y1,  $x12,$y12, !$colorflag, $depth);
        $depth =
        Sierpinski($cc, $x20,$y20, $x12,$y12, $x2, $y2,  !$colorflag, $depth);
    }
    $depth;
}
