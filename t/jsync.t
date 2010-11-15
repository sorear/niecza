# vim: ft=perl6
use Test;

sub to-jsync($obj) { Q:CgOp { (box Str (to_jsync (@ {$obj}))) } }
sub from-jsync($obj) { Q:CgOp { (ns (from_jsync (unbox str (@ {$obj})))) } }

class Bob { has $.abc; has $.def };

# Test set 1: proper rendering of native data types

sub myis($a,$b,$name) {
    if $a ne $b {
        say "# got : $a";
        say "# want: $b";
    }
    is $a, $b, $name;
}

sub t1($obj,$jsync,$name) {
    myis to-jsync($obj), $jsync, $name;
}
sub t2($obj,$jsync,$name) { t1($obj, '[{"%JSYNC":"1.0"},' ~ $jsync ~ ']', $name) }

t2 Any, 'null', 'null (Any)';
t2 Num, 'null', 'null (Num)';
t2 Str, 'null', 'null (Str)';
t2 Hash, 'null', 'null (Hash)';
t2 Array, 'null', 'null (Array)';

t2 1, '1', 'Num';
t2 (1/2), '0.5', 'Num (fractional)';

t1 [1,2,3], '["&A0",1,2,3]', 'Array';
t1 [[1,2],[3,4]], '["&A0",["&A1",1,2],["&A2",3,4]]', 'Array (nested)';
t1 do { my $a = [1,2]; [ $a, $a ] }, '["&A0",["&A1",1,2],"*A1"]',
    'Array (shared substructure)';
t1 do { my $a = []; $a.push($a); $a }, '["&A0","*A0"]',
    'Array (recursive)';

t1 { a => 5, b => 9 }, '{"&":"A0","a":5,"b":9}', 'Hash';
t1 { b => 5, a => 9 }, '{"&":"A0","a":9,"b":5}', 'Hash sorts keys';

t2 True, 'true', 'Bool::True';
t2 False, 'false', 'Bool::False';

t2 'a', '"a"', 'Str';
t2 '.&12', '"..&12"', 'Str (stuffing .&)';
t2 '*12', '".*12"', 'Str (stuffing *)';
t2 '%12', '".%12"', 'Str (stuffing %)';
t2 '!12', '".!12"', 'Str (stuffing !)';
t2 '..12', '"..12"', 'Str (not stuffing .)';
t2 "\x0A", '"\n"', 'Str (escaping LF)';
t2 "\x0D", '"\r"', 'Str (escaping CR)';
t2 "\x09", '"\t"', 'Str (escaping HT)';
t2 "\x0C", '"\f"', 'Str (escaping FF)';
t2 "\x08", '"\b"', 'Str (escaping BS)';
t2 "\x13", '"\u0013"', 'Str (escaping other C0)';
t2 "\x83", '"\u0083"', 'Str (escaping other C1)';

t1 do { my $obj = Bob.new; $obj.def = 15; $obj },
    '{"&":"A0","!":".!perl6/Bob","abc":null,"def":15}',
    'User-defined types with tags';

# Test set 2: parser

my $data = from-jsync(slurp "jsync.data");
$data.shift; # doc blob

for @$data -> $vector {
    my ($id, $text, $canon, $comment) = @$vector;
    my $result = "";
    try { $result = to-jsync(from-jsync($text)) }

    if $result eq "" && $canon ne "" {
        say "# $!";
    }

    myis $result, $canon, "($id) $comment";
}

done-testing;
