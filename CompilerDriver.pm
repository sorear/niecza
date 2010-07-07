use strict;
use warnings;
use 5.010;

use Body ();
use Unit ();
use Op ();

use Niecza::Grammar ();
use Niecza::Actions ();

Niecza::Grammar->parsefile("setting", setting => 'NULL', actions => 'Niecza::Actions')->{_ast}->write;
