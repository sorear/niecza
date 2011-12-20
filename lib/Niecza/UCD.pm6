module Niecza::UCD;

sub ranges_num($table, $matcher) is export {
    Q:CgOp { (ucd_get_ranges {$table} {$matcher}) }
}

sub get_value_num($table, $ord) is export {
    Q:CgOp { (ucd_get_value {$table} {$ord}) }
}

sub ranges($table, $matcher) is export {
    my @ranges;
    for ranges_num($table, $matcher) -> $low, $limit {
        push @ranges, ($limit == $low + 1) ?? chr($low) !!
            $(chr($low) .. chr($limit-1));
    }
    @ranges
}

sub value($table, $chr) is export { get_value_num($table, ord $chr) }

sub get_codepoint($name) is export { Q:CgOp { (ucd_get_codepoint {$name}) } }
