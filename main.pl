# the following was adapted from Geoffrey Broadwell's code in Rakudo nom
# * $?USAGE
#   * Create $?USAGE at compile time
#   * Make $?USAGE available globally
# * Command-line parsing
#   * Allow both = and space before argument of double-dash args
#   * Comma-separated list values
#   * Allow exact Perl 6 forms, quoted away from shell
# * Fix remaining XXXX

# TODO (sorear): add True, False to val(); (eval) becomes -e; change param
# names to be name-of-var or ""; Str.perl escaping

multi MAIN('foo', :$sam) { say "A" }
multi MAIN('bar', $quux) { say "B", $quux };
