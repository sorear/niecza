# gtk-tetris.pl for Niecza Perl 6
# Docs: http://en.wikipedia.org/wiki/Tetris

constant $GTK  = "gtk-sharp,  Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
constant $GDK  = "gdk-sharp,  Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";
constant $GLIB = "glib-sharp, Version=2.12.0.0, Culture=neutral, PublicKeyToken=35e10195dab3c99f";

# Class names that occur more than once.  Classes used only once appear inline.
constant Application    = CLR::("Gtk.Application,$GTK");
constant GLibTimeout    = CLR::("GLib.Timeout,$GLIB");

# Application data
constant matrixRows = 20; constant matrixColumns = 10;
my @tetrominoes =  # cell coordinates relative to the "middle" cell
    [ -1,  0,  0, 0, 1, 0, 2, 0 ],  # I
    [ -1,  0,  0, 0, 1, 0, 1, 1 ],  # J
    [ -1,  1, -1, 0, 0, 0, 1, 0 ],  # L
    [ -1,  1, -1, 0, 0, 0, 0, 1 ],  # O
    [ -1,  1,  0, 1, 0, 0, 1, 0 ],  # S
    [ -1,  0,  0, 0, 0, 1, 1, 0 ],  # T
    [ -1,  0,  0, 0, 0, 1, 1, 1 ];  # Z
my @colors =  # Copied approximately from the PC version
    [0,       0,       0      ], # 0: black background
    [0.5.Num, 0,       0      ], # 1: I maroon
    [1,       1,       1      ], # 2: J white
    [0.9.Num, 0,       0.9.Num], # 3: L dark magenta
    [0,       0,       0.6.Num], # 4: O dark blue
    [0,       0.8.Num, 0      ], # 5: S green
    [0.7.Num, 0.7.Num, 0      ], # 6: T brown
    [0,       0.8.Num, 0.8.Num]; # 7: Z dark cyan
my @matrix;
for ^matrixRows -> $i { for ^matrixColumns -> $j { @matrix[$i][$j] = 0; } }
my $pieceX; my $pieceY; my $colorindex; my @piece; CreatePiece();
my $oldInterval = 300;
my $newInterval = 300;
my $scale; my $offsetX; my $offsetY;

# ---------------------------- Main program ----------------------------
Application.Init;
my $window = CLR::("Gtk.Window,$GTK").new("tetris");
$window.SetDefaultSize(300, 500);
my $drawingarea = CLR::("Gtk.DrawingArea,$GTK").new;
$window.Add($drawingarea);
$window.add_DeleteEvent(sub ($obj,$args){$obj;$args;Application.Quit;});
$drawingarea.CanFocus = True;  # let arrow keys create KeyPressEvents
$drawingarea.add_ExposeEvent(&ExposeEvent);
$drawingarea.add_KeyPressEvent(&KeyPressEvent);
$window.ShowAll;
GLibTimeout.Add($newInterval, &TimeoutEvent);
Application.Run;

# --------------------------- Event handlers ---------------------------
sub TimeoutEvent()
{
    $drawingarea.QueueDraw;
    my $intervalSame = ($newInterval == $oldInterval);
    unless $intervalSame { GLibTimeout.Add($newInterval, &TimeoutEvent); }
    return $intervalSame;  # True means continue calling this timeout handler
}

sub KeyPressEvent($sender, $eventargs)  #OK not used
{
    given $eventargs.Event.Key {
        when 'Up'    { if $colorindex != 4 { TryRotatePiece() } }
        when 'Down'  { while CanMovePiece(0,1) {++$pieceY;} }
        when 'Left'  { if CanMovePiece(-1,0)   {--$pieceX;} }
        when 'Right' { if CanMovePiece( 1,0)   {++$pieceX;} }
    }
    return True;  # means this keypress is now handled
}

sub ExposeEvent($sender, $eventargs)  #OK not used
{
    my $cc = CLR::("Gdk.CairoHelper,$GDK").Create($sender.GdkWindow);  # Cairo Context
    my $windowX=0; my $windowY=0; my $windowWidth=0; my $windowHeight=0; my $windowDepth=0;
    $sender.GdkWindow.GetGeometry($windowX, $windowY, $windowWidth, $windowHeight, $windowDepth);
    $scale = ((($windowWidth / matrixColumns) min ($windowHeight / matrixRows)) * 0.95).Int;
    $offsetX = (($windowWidth  - ($scale * matrixColumns))/2).Int;
    $offsetY = (($windowHeight - ($scale * matrixRows   ))/2).Int;
    $cc.SetSourceRGB(0, 0, 0.4.Num); $cc.Paint;  # Dark blue background
    TryMovePieceDown();  # also clears full rows and makes new pieces
    DrawMatrix($cc);
    DrawPiece($cc);
    $cc.dispose-hack; # Should be $cc.IDisposable.Dispose but currently
    # CLR interop cannot call explicitly defined interface methods.
    # Tracked as https://github.com/sorear/niecza/issues/56
}

# ------------------------- Helper subroutines -------------------------
sub CreatePiece()
{
    my $piece = (rand * 7).Int % 7; # TODO: ^7.pick;
    $colorindex = $piece + 1;
    # TODO: @piece = @tetrominoes[$piece] instead of the following loop:
    @piece = (); for ^8 -> $i { @piece[$i] = @tetrominoes[$piece][$i]; }
    $pieceX = 4;
    $pieceY = 0;
}

sub CanMovePiece($deltaX, $deltaY)
{
    my $canMove = True;
    # Would any cell of the piece go below the bottom of the well,
    # or overlap existing cells lying there?
    for @piece -> $x, $y {
        if $x+$pieceX+$deltaX < 0 || $x+$pieceX+$deltaX >= matrixColumns ||
           $y+$pieceY+$deltaY < 0 || $y+$pieceY+$deltaY >= matrixRows ||
           @matrix[$y+$pieceY+$deltaY][$x+$pieceX+$deltaX] != 0
        {
            $canMove = False;
        }
    }
    $canMove;
}

sub TryMovePieceDown()
{
    if CanMovePiece(0, 1) {
        ++$pieceY;
    }
    else {  # Copy this piece into the matrix and start with a new piece
        for @piece -> $x, $y {
            @matrix[$y+$pieceY][$x+$pieceX] = $colorindex;
        }
        # Look for full rows and remove them. TODO: keep score.
        for ^matrixRows -> $row {
            my $full = True;
            for ^matrixColumns -> $column {
                if @matrix[$row][$column] == 0 { $full = False; }
            }
            if $full {
                loop (my $moveRow = $row; $moveRow > 0; --$moveRow) {
                    for ^matrixColumns -> $column {
                        @matrix[$moveRow][$column] = @matrix[$moveRow-1][$column];
                    }
                }
                for ^matrixColumns -> $column {
                    @matrix[0][$column] = 0;
                }
            }
        }
        CreatePiece();
    }
}

sub TryRotatePiece()
{
    my @p = @piece;
    @piece = @p[1], -@p[0], @p[3], -@p[2], @p[5], -@p[4], @p[7], -@p[6];
    if ! CanMovePiece(0, 0) {
        @piece = @p;
    }
}

sub DrawMatrix($cc)
{
    my $x=0; my $y=0;
    for ^matrixRows -> $row {
        for ^matrixColumns -> $column {
            $cc.SetSourceRGB(|@colors[@matrix[$row][$column]]);
            $cc.Rectangle($x+$offsetX,$y+$offsetY,$scale,$scale);
            $cc.Fill;
            $x += $scale;
        }
        $x = 0;
        $y += $scale;
    }
    $cc.Stroke;
}

sub DrawPiece($cc)
{
    $cc.SetSourceRGB(|@colors[$colorindex]);
    for @piece -> $x, $y {
        $cc.Rectangle(($pieceX+$x)*$scale+$offsetX,($pieceY+$y)*$scale+$offsetY,$scale,$scale);
        $cc.Fill;
    }
    $cc.Stroke;
}
