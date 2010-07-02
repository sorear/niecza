use strict;
use warnings;
use 5.010;

use Body ();
use Unit ();
use Op ();

my $unit = Unit->new(
    mainline => Body->new(
        name   => 'body',
        protos => [
            # we have to do this directly due to the circularity saw.  same
            # reason forces uncontainerized .NET values
            # class ClassHOW {
            #     has Array[ClassHOW] $.parents;
            #     has Dictionary[string,Sub] $.local-methods;
            #     has DynMetaObject $.meta;
            #     has DynObject $.proto;
            #
            #     sub 
            # }
            [ 1, 'ClassHOW' => Body->new(
                    protos => [
                        [ 0, '&wrap-metaclass', Body->new(do => Op::NIL->new(code => [
                            # Args: 0: metaclass metaobject 1: metaobject
                            # returns: protoobject
                            ['lextypes', 'mci', 'DynObject', 'p', 'DynObject',
                                'mo', 'DynMetaObject'],
                            ['clr_new', 'DynObject', 0],
                            ['rawlexput', 'mci'],
                            ['pos',1],
                            ['fetchlv'],
                            ['clr_unwrap', 'DynMetaObject'],
                            ['rawlexput', 'mo'],
                            ['clr_new', 'DynObject', 0],
                            ['rawlexput', 'p'],

                            ['rawlexget', 'p'],
                            ['rawlexget', 'mo'],
                            ['clr_field_set', 'klass'],

                            ['rawlexget', 'mo'],
                            ['rawlexget', 'mci'],
                            ['clr_field_set', 'how'],

                            ['rawlexget', 'mci'],
                            ['clr_field_get', 'Dictionary<string,object>', 'slots'],
                            ['rawlexget', 'mo'],
                            ['clr_index_set', 'meta-object'],

                            ['rawlexget', 'mci'],
                            ['clr_field_get', 'Dictionary<string,object>', 'slots'],
                            ['rawlexget', 'p'],
                            ['clr_index_set', 'prototype'],

                            ['rawlexget', 'mci'],
                            ['pos',0],
                            ['fetchlv'],
                            ['clr_unwrap', 'DynMetaObject'],
                            ['clr_field_set', 'klass'],

                            ['rawlexget', 'p'],
                            ['clr_call_direct', 'Variable', 'Kernel.NewROVar', 1]]))],
                        [ 0, '&new-metaclass', Body->new(do => Op::NIL->new(code => [
                            # Args: 0: metaclass metaobject, 1: name
                            ['lextypes','mo','DynMetaObject'],
                            ['clr_new','DynMetaObject',0],
                            ['rawlexput','mo'],

                            ['rawlexget','mo'],
                            ['pos',1],
                            ['fetchlv'],
                            ['clr_unwrap','String'],
                            ['clr_field_set','name'],

                            ['lex_lv',1,'&wrap-metaclass'],
                            ['fetchlv'],
                            ['pos',0],
                            ['rawlexget','mo'],
                            ['clr_new','CLRImportObject',1],
                            ['clr_call_direct','LValue','Kernel.NewROLValue',1],
                            ['tail_call_sub', 2]]))],
                        [ 0, '&new', Body->new(do => Op::NIL->new(code => [
                            ['lex_lv',1,'&new-metaclass'],
                            ['fetchlv'],
                            ['pos',0],
                            ['fetchlv'],
                            ['cast','DynObject'],
                            ['clr_field_get','DynMetaObject','klass'],
                            ['clr_new','CLRImportObject',1],
                            ['clr_call_direct','LValue','Kernel.NewROLValue',1],
                            ['pos',1],
                            ['tail_call_sub',2]]))],
                        [ 0, '&add-method', Body->new(do => Op::NIL->new(code => [
                            ['push_null','Variable'],
                            ]))]],
                    do => Op::NIL->new(code => [
                        ['lextypes','$p','Variable'],
                        ['copy_lex','&new-metaclass'],
                        ['lex_lv',0,'&new-metaclass'],
                        ['fetchlv'],
                        ['push_null','DynMetaObject'],
                        ['clr_new','CLRImportObject',1],
                        ['clr_call_direct','LValue','Kernel.NewROLValue',1],
                        ['string_lv','ClassHOW'],
                        ['call_sub',1,2],
                        ['rawlexput','$p'],

                        ['lex_lv',0,'$p'],
                        ['fetchlv'],
                        ['how'],
                        ['cast','DynObject'],
                        ['lex_lv',0,'$p'],
                        ['fetchlv'],
                        ['cast','DynObject'],
                        ['clr_field_get','DynMetaObject','klass'],
                        ['clr_field_set','klass'],

                        ['rawlexget','$p']]))],
            [ 0, '&say' => Body->new(
                    do => Op::NIL->new(
                        code => [
                            ['pos', 0],
                            ['fetchlv'],
                            ['clr_unwrap', 'System.String'],
                            ['clr_call_direct', undef,
                                "System.Console.WriteLine", 1],
                            ['push_null', 'Variable']])) ]],
        enter  => [Op::CloneSub->new(name => '&say')],
        lexical=> { '&say', 1 },
        do     => Op::CallSub->new(
            invocant    => Op::Lexical->new(name => '&say'),
            positionals => [Op::StringLiteral->new(text => 'Hello, World')])));

$unit->write;
