# vim: ft=perl6
use Test;

sub to-jsync($obj) { Q:CgOp { (box Str (to_jsync (@ {$obj}))) } }
sub from-jsync($obj) { Q:CgOp { (ns (from_jsync (unbox str (@ {$obj})))) } }

for $*IN.lines -> $jsync { say to-jsync(from-jsync($jsync)) }

# say to-jsync 1;
# say to-jsync (1 / 2);
# say to-jsync "foo";
# say to-jsync [1,2,3];
# say to-jsync { a => 5, b => 9 };
# say to-jsync True;
# say to-jsync '.&12';
# my $obj = []; $obj.push($obj);
# say to-jsync $obj;
# class Bob { has $.foo };
# $obj = Bob.new;
# $obj.foo = 15;
# say to-jsync $obj;
