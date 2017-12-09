// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List                    = require("bs-platform/lib/js/list.js");
var Curry                   = require("bs-platform/lib/js/curry.js");
var $$String                = require("bs-platform/lib/js/string.js");
var Pervasives              = require("bs-platform/lib/js/pervasives.js");
var Caml_format             = require("bs-platform/lib/js/caml_format.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function explode(s) {
  var n = s.length;
  if (n !== 0) {
    var _i = n - 1 | 0;
    var _acc = /* [] */0;
    while(true) {
      var acc = _acc;
      var i = _i;
      var acc_000 = $$String.sub(s, i, 1);
      var acc$1 = /* :: */[
        acc_000,
        acc
      ];
      if (i !== 0) {
        _acc = acc$1;
        _i = i - 1 | 0;
        continue ;
        
      } else {
        return acc$1;
      }
    };
  } else {
    return /* [] */0;
  }
}

function filterWithIndexAndListAndLength(f, l) {
  return List.map((function (a) {
                if (a) {
                  return a[0];
                } else {
                  throw [
                        Caml_builtin_exceptions.assert_failure,
                        [
                          "day1.re",
                          28,
                          19
                        ]
                      ];
                }
              }), List.filter((function (a) {
                      if (a) {
                        return /* true */1;
                      } else {
                        return /* false */0;
                      }
                    }))(List.mapi((function (i, a) {
                        var match = Curry._4(f, i, a, l, List.length(l));
                        if (match !== 0) {
                          return /* Some */[a];
                        } else {
                          return /* None */0;
                        }
                      }), l)));
}

function solution(input) {
  return Pervasives.string_of_int(List.fold_left((function (acc, n) {
                    return acc + n | 0;
                  }), 0, filterWithIndexAndListAndLength((function (i, n, ns, length) {
                        var match = +(i < (length - 1 | 0));
                        if (match !== 0) {
                          return +(n === List.nth(ns, i + 1 | 0));
                        } else {
                          return +(n === List.nth(ns, 0));
                        }
                      }), List.map(Caml_format.caml_int_of_string, explode(input)))));
}

exports.explode                         = explode;
exports.filterWithIndexAndListAndLength = filterWithIndexAndListAndLength;
exports.solution                        = solution;
/* No side effect */