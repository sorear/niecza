# Main documentation: http://docs.go-mono.com, particularly Gnome
# (for Gdk and Gtk) and Mono (for Cairo) libraries.
# See also: The X-Windows Disaster at http://www.art.net/~hopkins/Don/unix-haters/handbook.html

constant $GTK = "gtk-sharp,Version=2.12.0.0,Culture=neutral,PublicKeyToken=35e10195dab3c99f";
constant $GDK = "gdk-sharp, Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
constant $GLIB = "glib-sharp, Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
# use 'gacutil -l' to look up similar module details

constant Application    = CLR::("Gtk.Application,$GTK");
constant Window         = CLR::("Gtk.Window,$GTK");
constant GdkCairoHelper = CLR::("Gdk.CairoHelper,$GDK");
constant GtkDrawingArea = CLR::("Gtk.DrawingArea,$GTK");
constant GLibTimeout    = CLR::("GLib.Timeout,$GLIB");

Application.Init;
my $window = Window.new("clock");
my $windowSizeX = 200; my $windowSizeY = 200;
$window.Resize($windowSizeX, $windowSizeY);
my $drawingarea = GtkDrawingArea.new;
$drawingarea.add_ExposeEvent: sub ($obj, $args) {
    $args;  # suppress 'declared but not used' warnings
    my $cc = GdkCairoHelper.Create($obj.GdkWindow);  # Cairo Context
    my $windowX; my $windowY; my $windowWidth; my $windowHeight; my $windowDepth;
    # TODO: the following line seems to pass parameters by value, it needs to pass by reference to integers
    # Tracked as https://github.com/sorear/niecza/issues/57
    $obj.GdkWindow.GetGeometry($windowX, $windowY, $windowWidth, $windowHeight, $windowDepth);
    # TODO: remove the following cheat that works around the above problem
    $windowWidth = $windowSizeX; $windowHeight = $windowSizeY;
    my $xCenter = $windowWidth / 2; my $yCenter = $windowHeight / 2;
    my $maxRadius = ($windowWidth min $windowHeight)/2;
    $cc.SetSourceRGB(0, 1, 1); $cc.Paint();  # Make a cyan background
    ClockFace($cc, $windowWidth, $windowHeight);
    # Calculate the parameters needed to draw the clock
    my $hour = ((now.to-posix[0] % (12*3600))/3600); # there must be a more elegant way...
    my $minute = ((now.to-posix[0] / 60) % 60);
    my $linewidth = ($maxRadius * 0.03).Int max 1;
    my $radiusHour = $maxRadius * 0.5;
    my $radiusMinute = $maxRadius * 0.7;
    # Draw the hands of the clock
    my $radiansHour = $hour / 12 * pi * 2;
    my $radiansMinute = $minute / 60 * pi * 2;
    my $xHour = $xCenter + $radiusHour * sin($radiansHour);
    my $yHour = $yCenter + $radiusHour * sin($radiansHour - pi / 2);
    my $xMinute = $xCenter + $radiusMinute * sin($radiansMinute);
    my $yMinute = $yCenter + $radiusMinute * sin($radiansMinute - pi / 2);
    $cc.LineWidth = $linewidth;
    $cc.MoveTo($xMinute.Int,$yMinute.Int);
    $cc.LineTo($xCenter.Int,$yCenter.Int);
    $cc.LineTo($xHour.Int,$yHour.Int);
    $cc.Arc($xCenter.Int,$yCenter.Int,$linewidth.Int,0,7); # from 0 radians to more than 2*pi 
    $cc.Stroke;
    $cc.Target.Dispose;
    # TODO: Currently the following error appears at exit:
    # Cairo.Context: called from finalization thread, programmer is missing a call to Dispose
    # The following line should fix the above, but gives this error: Unable to resolve method Dispose
    # $cc.Dispose(); # Should do: ((IDisposable) cc).Dispose();
    # Tracked as https://github.com/sorear/niecza/issues/56
};
GLibTimeout.Add: 60000, sub () {  # Update once per minute
    $drawingarea.QueueDrawArea(0,0,$windowSizeX,$windowSizeY);
    return True;  # meaning please continue calling this timeout handler
};
$window.add_DeleteEvent: sub ($obj, $args) {
    $obj; $args; # suppress "declared but not used" Potential difficulties
    Application.Quit;
};
$window.Add($drawingarea);
$window.ShowAll;
Application.Run;

sub ClockFace($cc, $width, $height)
{
    # Calculate dimensions relative to window size
    my $maxRadius = ($width min $height) / 2;
    my $centerX = $width / 2;
    my $centerY = $height / 2;
    my $lineWidth = ($maxRadius * 0.025).Int max 1;
    # Draw the outer circle
    $cc.SetSourceRGB(0, 0, 0);  # Black
    $cc.LineWidth = $lineWidth;
    $cc.Arc($centerX.Int, $centerY.Int, ($maxRadius - $lineWidth).Int, 0, 7);
    $cc.Stroke();
    # Write the clock name on its face
    my $clockName = "it's Niecza time!";
    $cc.SetSourceRGB(0, 0, 0);  # Black
    $cc.SetFontSize(($maxRadius * 0.15).Int);
    my $textWidth = $cc.TextExtents($clockName).Width;
    $cc.MoveTo(($width/2 - $textWidth/2).Int, ($height * 0.85).Int);
    $cc.ShowText($clockName);
    $cc.Stroke();
}
