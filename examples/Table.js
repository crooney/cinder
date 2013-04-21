/** @constructor
*/
var Table = function(){
/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function Fay$$_(thunkish,nocache){
  while (thunkish instanceof Fay$$$) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function Fay$$__(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof Fay$$$? Fay$$_(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function Fay$$$(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
Fay$$$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};


function Fay$$seq(x) {
  return function(y) {
    Fay$$_(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  Fay$$_(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new Fay$$$(function(){
      var monad = Fay$$_(m,true);
      return Fay$$_(f)(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
  return new Fay$$$(function(){
    var monad = Fay$$_(m,true);
    return Fay$$_(f)(monad.value);
  });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new Fay$$$(function(){
      Fay$$_(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  if(base == "action") {
    // A nullary monadic action. Should become a nullary JS function.
    // Fay () -> function(){ return ... }
    jsObj = function(){
      return Fay$$fayToJs(args[0],Fay$$_(fayObj,true).value);
    };

  }
  else if(base == "function") {
    // A proper function.
    jsObj = function(){
      var fayFunc = fayObj;
      var return_type = args[args.length-1];
      var len = args.length;
      // If some arguments.
      if (len > 1) {
        // Apply to all the arguments.
        fayFunc = Fay$$_(fayFunc,true);
        // TODO: Perhaps we should throw an error when JS
        // passes more arguments than Haskell accepts.
        for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
          // Unserialize the JS values to Fay for the Fay callback.
          fayFunc = Fay$$_(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
        }
        // Finally, serialize the Fay return value back to JS.
        var return_base = return_type[0];
        var return_args = return_type[1];
        // If it's a monadic return value, get the value instead.
        if(return_base == "action") {
          return Fay$$fayToJs(return_args[0],fayFunc.value);
        }
        // Otherwise just serialize the value direct.
        else {
          return Fay$$fayToJs(return_type,fayFunc);
        }
      } else {
        throw new Error("Nullary function?");
      }
    };

  }
  else if(base == "string") {
    jsObj = Fay$$fayToJs_string(fayObj);
  }
  else if(base == "list") {
    // Serialize Fay list to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[0],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    jsObj = arr;

  }
  else if(base == "tuple") {
    // Serialize Fay tuple to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    var i = 0;
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[i++],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    jsObj = arr;

  }
  else if(base == "defined") {
    fayObj = Fay$$_(fayObj);
    if (fayObj instanceof $_Language$Fay$FFI$Undefined) {
      jsObj = undefined;
    } else {
      jsObj = Fay$$fayToJsUserDefined(args[0],fayObj["slot1"]);
    }

  }
  else if(base == "nullable") {
    fayObj = Fay$$_(fayObj);
    if (fayObj instanceof $_Language$Fay$FFI$Null) {
      jsObj = null;
    } else {
      jsObj = Fay$$fayToJsUserDefined(args[0],fayObj["slot1"]);
    }

  }
  else if(base == "double" || base == "int" || base == "bool") {
    // Bools are unboxed.
    jsObj = Fay$$_(fayObj);

  }
  else if(base == "ptr" || base == "unknown")
    return fayObj;
  else if(base == "automatic" || base == "user") {
    if(fayObj instanceof Fay$$$)
      fayObj = Fay$$_(fayObj);
    jsObj = Fay$$fayToJsUserDefined(type,fayObj);

  }
  else
    throw new Error("Unhandled Fay->JS translation type: " + base);
  return jsObj;
}

// Specialized serializer for string.
function Fay$$fayToJs_string(fayObj){
  // Serialize Fay string to JavaScript string.
  var str = "";
  fayObj = Fay$$_(fayObj);
  while(fayObj instanceof Fay$$Cons) {
    str += fayObj.car;
    fayObj = Fay$$_(fayObj.cdr);
  }
  return str;
};
function Fay$$jsToFay_string(x){
  return Fay$$list(x)
};

// Special num/bool serializers.
function Fay$$jsToFay_int(x){return x;}
function Fay$$jsToFay_double(x){return x;}
function Fay$$jsToFay_bool(x){return x;}

function Fay$$fayToJs_int(x){return Fay$$_(x);}
function Fay$$fayToJs_double(x){return Fay$$_(x);}
function Fay$$fayToJs_bool(x){return Fay$$_(x);}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  if(base == "action") {
    // Unserialize a "monadic" JavaScript return value into a monadic value.
    fayObj = new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));

  }
  else if(base == "string") {
    // Unserialize a JS string into Fay list (String).
    fayObj = Fay$$list(jsObj);
  }
  else if(base == "list") {
    // Unserialize a JS array into a Fay list ([a]).
    var serializedList = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
    }
    // Pop it all in a Fay list.
    fayObj = Fay$$list(serializedList);

  }
  else if(base == "tuple") {
    // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
    var serializedTuple = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
    }
    // Pop it all in a Fay list.
    fayObj = Fay$$list(serializedTuple);

  }
  else if(base == "defined") {
    if (jsObj === undefined) {
      fayObj = new $_Language$Fay$FFI$Undefined();
    } else {
      fayObj = new $_Language$Fay$FFI$Defined(Fay$$jsToFay(args[0],jsObj));
    }

  }
  else if(base == "nullable") {
    if (jsObj === null) {
      fayObj = new $_Language$Fay$FFI$Null();
    } else {
      fayObj = new $_Language$Fay$FFI$Nullable(Fay$$jsToFay(args[0],jsObj));
    }

  }
  else if(base == "int") {
    // Int are unboxed, so there's no forcing to do.
    // But we can do validation that the int has no decimal places.
    // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
    fayObj = Math.round(jsObj);
    if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";

  }
  else if (base == "double" ||
           base == "bool" ||
           base ==  "ptr" ||
           base ==  "unknown") {
    return jsObj;
  }
  else if(base == "automatic" || base == "user") {
    if (jsObj && jsObj['instance']) {
      fayObj = Fay$$jsToFayUserDefined(type,jsObj);
    }
    else
      fayObj = jsObj;

  }
  else { throw new Error("Unhandled JS->Fay translation type: " + base); }
  return fayObj;
}

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
// `list' is already forced by the time it's passed to this function.
// `list' cannot be null and `index' cannot be out of bounds.
function Fay$$index(index,list){
  for(var i = 0; i < index; i++) {
    list = Fay$$_(list.cdr);
  }
  return list.car;
}

// List length.
// `list' is already forced by the time it's passed to this function.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = Fay$$_(list.cdr);
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) * Fay$$_(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) * Fay$$_(y);
  });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) + Fay$$_(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) + Fay$$_(y);
  });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) - Fay$$_(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) - Fay$$_(y);
  });

}

// Built-in /.
function Fay$$divi(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) / Fay$$_(y);
    });
  };
}

// Built-in /.
function Fay$$divi$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) / Fay$$_(y);
  });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = Fay$$_(lit1);
  lit2 = Fay$$_(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = Fay$$_(lit1.cdr), lit2 = Fay$$_(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.constructor === lit2.constructor) {
    for(var x in lit1) {
      if(!(lit1.hasOwnProperty(x) && lit2.hasOwnProperty(x) &&
           Fay$$equal(lit1[x],lit2[x])))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$equal(x,y);
  });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new Fay$$$(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return !(Fay$$equal(x,y));
  });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) > Fay$$_(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) > Fay$$_(y);
  });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) < Fay$$_(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) < Fay$$_(y);
  });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) >= Fay$$_(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) >= Fay$$_(y);
  });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) <= Fay$$_(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) <= Fay$$_(y);
  });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) && Fay$$_(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) && Fay$$_(y);
  });
  ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) || Fay$$_(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) || Fay$$_(y);
  });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */

var Language$Fay$FFI$Nullable = function(slot1){
  return new Fay$$$(function(){
    return new $_Language$Fay$FFI$Nullable(slot1);
  });
};
var Language$Fay$FFI$Null = new Fay$$$(function(){
  return new $_Language$Fay$FFI$Null();
});
var Language$Fay$FFI$Defined = function(slot1){
  return new Fay$$$(function(){
    return new $_Language$Fay$FFI$Defined(slot1);
  });
};
var Language$Fay$FFI$Undefined = new Fay$$$(function(){
  return new $_Language$Fay$FFI$Undefined();
});
var Prelude$Just = function(slot1){
  return new Fay$$$(function(){
    return new $_Prelude$Just(slot1);
  });
};
var Prelude$Nothing = new Fay$$$(function(){
  return new $_Prelude$Nothing();
});
var Prelude$Left = function(slot1){
  return new Fay$$$(function(){
    return new $_Prelude$Left(slot1);
  });
};
var Prelude$Right = function(slot1){
  return new Fay$$$(function(){
    return new $_Prelude$Right(slot1);
  });
};
var Prelude$maybe = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) instanceof $_Prelude$Nothing) {
          var m = $p1;
          return m;
        }
        if (Fay$$_($p3) instanceof $_Prelude$Just) {
          var x = Fay$$_($p3).slot1;
          var f = $p2;
          return Fay$$_(f)(x);
        }
        throw ["unhandled case in maybe",[$p1,$p2,$p3]];
      });
    };
  };
};
var Prelude$Ratio = function(slot1){
  return function(slot2){
    return new Fay$$$(function(){
      return new $_Prelude$Ratio(slot1,slot2);
    });
  };
};
var Prelude$$62$$62$$61$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$bind(Fay$$fayToJs(["action",[["unknown"]]],$p1))(Fay$$fayToJs(["function",[["unknown"],["action",[["unknown"]]]]],$p2))));
    });
  };
};
var Prelude$$62$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$then(Fay$$fayToJs(["action",[["unknown"]]],$p1))(Fay$$fayToJs(["action",[["unknown"]]],$p2))));
    });
  };
};
var Prelude$$_return = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$return(Fay$$fayToJs(["unknown"],$p1))));
  });
};
var Prelude$when = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var m = $p2;
      var p = $p1;
      return Fay$$_(p) ? Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Fay$$$_return)(Fay$$unit)) : Fay$$_(Fay$$$_return)(Fay$$unit);
    });
  };
};
var Prelude$forM_ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var m = $p2;
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude$forM_)(xs))(m));
      }
      if (Fay$$_($p1) === null) {
        return Fay$$_(Fay$$$_return)(Fay$$unit);
      }
      throw ["unhandled case in forM_",[$p1,$p2]];
    });
  };
};
var Prelude$mapM_ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var m = $p1;
        return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude$mapM_)(m))(xs));
      }
      if (Fay$$_($p2) === null) {
        return Fay$$_(Fay$$$_return)(Fay$$unit);
      }
      throw ["unhandled case in mapM_",[$p1,$p2]];
    });
  };
};
var Prelude$$61$$60$$60$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(Fay$$_(Fay$$bind)(x))(f);
    });
  };
};
var Prelude$sequence = function($p1){
  return new Fay$$$(function(){
    var ms = $p1;
    return (function(){
      var k = function($p1){
        return function($p2){
          return new Fay$$$(function(){
            var m$39$ = $p2;
            var m = $p1;
            return Fay$$_(Fay$$_(Fay$$bind)(m))(function($p1){
              var x = $p1;
              return Fay$$_(Fay$$_(Fay$$bind)(m$39$))(function($p1){
                var xs = $p1;
                return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Fay$$cons)(x))(xs));
              });
            });
          });
        };
      };
      return Fay$$_(Fay$$_(Fay$$_(Prelude$foldr)(k))(Fay$$_(Fay$$$_return)(null)))(ms);
    })();
  });
};
var Prelude$sequence_ = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Fay$$$_return)(Fay$$unit);
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var m = $tmp1.car;
      var ms = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Prelude$sequence_)(ms));
    }
    throw ["unhandled case in sequence_",[$p1]];
  });
};
var Prelude$GT = new Fay$$$(function(){
  return new $_Prelude$GT();
});
var Prelude$LT = new Fay$$$(function(){
  return new $_Prelude$LT();
});
var Prelude$EQ = new Fay$$$(function(){
  return new $_Prelude$EQ();
});
var Prelude$compare = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(y)) ? Prelude$GT : Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(y)) ? Prelude$LT : Prelude$EQ;
    });
  };
};
var Prelude$succ = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$add)(x))(1);
  });
};
var Prelude$pred = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$sub)(x))(1);
  });
};
var Prelude$enumFrom = function($p1){
  return new Fay$$$(function(){
    var i = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Prelude$enumFrom)(Fay$$_(Fay$$_(Fay$$add)(i))(1)));
  });
};
var Prelude$enumFromTo = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var n = $p2;
      var i = $p1;
      return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(i))(n)) ? null : Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Fay$$_(Prelude$enumFromTo)(Fay$$_(Fay$$_(Fay$$add)(i))(1)))(n));
    });
  };
};
var Prelude$enumFromBy = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var by = $p2;
      var fr = $p1;
      return Fay$$_(Fay$$_(Fay$$cons)(fr))(Fay$$_(Fay$$_(Prelude$enumFromBy)(Fay$$_(Fay$$_(Fay$$add)(fr))(by)))(by));
    });
  };
};
var Prelude$enumFromThen = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var th = $p2;
      var fr = $p1;
      return Fay$$_(Fay$$_(Prelude$enumFromBy)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr));
    });
  };
};
var Prelude$enumFromByTo = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var to = $p3;
        var by = $p2;
        var fr = $p1;
        return (function(){
          var neg = function($p1){
            return new Fay$$$(function(){
              var x = $p1;
              return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(neg)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));
            });
          };
          var pos = function($p1){
            return new Fay$$$(function(){
              var x = $p1;
              return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(pos)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));
            });
          };
          return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(by))(0)) ? Fay$$_(neg)(fr) : Fay$$_(pos)(fr);
        })();
      });
    };
  };
};
var Prelude$enumFromThenTo = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var to = $p3;
        var th = $p2;
        var fr = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Prelude$enumFromByTo)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr)))(to);
      });
    };
  };
};
var Prelude$fromIntegral = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Fay$$fayToJs_int($p1));
  });
};
var Prelude$fromInteger = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Fay$$fayToJs_int($p1));
  });
};
var Prelude$not = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(p) ? false : true;
  });
};
var Prelude$otherwise = true;
var Prelude$show = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_string(JSON.stringify(Fay$$fayToJs(["automatic"],$p1)));
  });
};
var Prelude$error = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());
  });
};
var Prelude$$_undefined = new Fay$$$(function(){
  return Fay$$_(Prelude$error)(Fay$$list("Prelude.undefined"));
});
var Prelude$either = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) instanceof $_Prelude$Left) {
          var a = Fay$$_($p3).slot1;
          var f = $p1;
          return Fay$$_(f)(a);
        }
        if (Fay$$_($p3) instanceof $_Prelude$Right) {
          var b = Fay$$_($p3).slot1;
          var g = $p2;
          return Fay$$_(g)(b);
        }
        throw ["unhandled case in either",[$p1,$p2,$p3]];
      });
    };
  };
};
var Prelude$until = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var x = $p3;
        var f = $p2;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? x : Fay$$_(Fay$$_(Fay$$_(Prelude$until)(p))(f))(Fay$$_(f)(x));
      });
    };
  };
};
var Prelude$$36$$33$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(Fay$$_(Fay$$seq)(x))(Fay$$_(f)(x));
    });
  };
};
var Prelude$$_const = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var a = $p1;
      return a;
    });
  };
};
var Prelude$id = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return x;
  });
};
var Prelude$$46$ = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var x = $p3;
        var g = $p2;
        var f = $p1;
        return Fay$$_(f)(Fay$$_(g)(x));
      });
    };
  };
};
var Prelude$$36$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(f)(x);
    });
  };
};
var Prelude$flip = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var y = $p3;
        var x = $p2;
        var f = $p1;
        return Fay$$_(Fay$$_(f)(y))(x);
      });
    };
  };
};
var Prelude$curry = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var y = $p3;
        var x = $p2;
        var f = $p1;
        return Fay$$_(f)(Fay$$list([x,y]));
      });
    };
  };
};
var Prelude$uncurry = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var p = $p2;
      var f = $p1;
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var x = Fay$$index(0,Fay$$_($tmp1));
          var y = Fay$$index(1,Fay$$_($tmp1));
          return Fay$$_(Fay$$_(f)(x))(y);
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(p);
    });
  };
};
var Prelude$snd = function($p1){
  return new Fay$$$(function(){
    if (Fay$$listLen(Fay$$_($p1),2)) {
      var x = Fay$$index(1,Fay$$_($p1));
      return x;
    }
    throw ["unhandled case in snd",[$p1]];
  });
};
var Prelude$fst = function($p1){
  return new Fay$$$(function(){
    if (Fay$$listLen(Fay$$_($p1),2)) {
      var x = Fay$$index(0,Fay$$_($p1));
      return x;
    }
    throw ["unhandled case in fst",[$p1]];
  });
};
var Prelude$div = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {
        return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$quot)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(1);
      } else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {
          return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$quot)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(1);
        }
      }
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Prelude$quot)(x))(y);
    });
  };
};
var Prelude$mod = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {
        return Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude$rem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(y)))(1);
      } else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {
          return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude$rem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(y)))(1);
        }
      }
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Prelude$rem)(x))(y);
    });
  };
};
var Prelude$divMod = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var q = Fay$$index(0,Fay$$_($tmp1));
            var r = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Fay$$_(Prelude$quotRem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y));
      } else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(1)))) {
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var q = Fay$$index(0,Fay$$_($tmp1));
              var r = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Fay$$_(Prelude$quotRem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y));
        }
      }
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Prelude$quotRem)(x))(y);
    });
  };
};
var Prelude$min = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["unknown"],Math.min(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));
    });
  };
};
var Prelude$max = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["unknown"],Math.max(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));
    });
  };
};
var Prelude$recip = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(1))(x);
  });
};
var Prelude$negate = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (-(Fay$$_(x)));
  });
};
var Prelude$abs = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude$negate)(x) : x;
  });
};
var Prelude$signum = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(0)) ? 1 : Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(x))(0)) ? 0 : (-(1));
  });
};
var Prelude$pi = new Fay$$$(function(){
  return Fay$$jsToFay_double(Math.PI);
});
var Prelude$exp = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$sqrt = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$log = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$$42$$42$ = new Fay$$$(function(){
  return Prelude$unsafePow;
});
var Prelude$$94$$94$ = new Fay$$$(function(){
  return Prelude$unsafePow;
});
var Prelude$unsafePow = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay(["unknown"],Math.pow(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));
    });
  };
};
var Prelude$$94$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      if (Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0))) {
        return Fay$$_(Prelude$error)(Fay$$list("(^): negative exponent"));
      } else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(b))(0))) {
          return 1;
        } else {if (Fay$$_(Fay$$_(Prelude$even)(b))) {
            return (function(){
              var x = new Fay$$$(function(){
                return Fay$$_(Fay$$_(Prelude$$94$)(a))(Fay$$_(Fay$$_(Prelude$quot)(b))(2));
              });
              return Fay$$_(Fay$$_(Fay$$mult)(x))(x);
            })();
          }
        }
      }
      var b = $p2;
      var a = $p1;
      return Fay$$_(Fay$$_(Fay$$mult)(a))(Fay$$_(Fay$$_(Prelude$$94$)(a))(Fay$$_(Fay$$_(Fay$$sub)(b))(1)));
    });
  };
};
var Prelude$logBase = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var b = $p1;
      return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude$log)(x)))(Fay$$_(Prelude$log)(b));
    });
  };
};
var Prelude$sin = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.sin(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$tan = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.tan(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$cos = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.cos(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$asin = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.asin(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$atan = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.atan(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$acos = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_double(Math.acos(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$sinh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Prelude$exp)(x)))(Fay$$_(Prelude$exp)((-(Fay$$_(x)))))))(2);
  });
};
var Prelude$tanh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (function(){
      var a = new Fay$$$(function(){
        return Fay$$_(Prelude$exp)(x);
      });
      var b = new Fay$$$(function(){
        return Fay$$_(Prelude$exp)((-(Fay$$_(x))));
      });
      return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(a))(b)))(Fay$$_(Fay$$_(Fay$$add)(a))(b));
    })();
  });
};
var Prelude$cosh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Prelude$exp)(x)))(Fay$$_(Prelude$exp)((-(Fay$$_(x)))))))(2);
  });
};
var Prelude$asinh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Prelude$log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude$sqrt)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude$$42$$42$)(x))(2)))(1))));
  });
};
var Prelude$atanh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude$log)(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(1))(x)))(Fay$$_(Fay$$_(Fay$$sub)(1))(x)))))(2);
  });
};
var Prelude$acosh = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Prelude$log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude$sqrt)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$$42$$42$)(x))(2)))(1))));
  });
};
var Prelude$properFraction = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return (function(){
      var a = new Fay$$$(function(){
        return Fay$$_(Prelude$truncate)(x);
      });
      return Fay$$list([a,Fay$$_(Fay$$_(Fay$$sub)(x))(Fay$$_(Prelude$fromIntegral)(a))]);
    })();
  });
};
var Prelude$truncate = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude$ceiling)(x) : Fay$$_(Prelude$floor)(x);
  });
};
var Prelude$round = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.round(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$ceiling = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$floor = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));
  });
};
var Prelude$subtract = new Fay$$$(function(){
  return Fay$$_(Prelude$flip)(Fay$$sub);
});
var Prelude$even = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$eq)(Fay$$_(Fay$$_(Prelude$rem)(x))(2)))(0);
  });
};
var Prelude$odd = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Prelude$not)(Fay$$_(Prelude$even)(x));
  });
};
var Prelude$gcd = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      return (function(){
        var go = function($p1){
          return function($p2){
            return new Fay$$$(function(){
              if (Fay$$_($p2) === 0) {
                var x = $p1;
                return x;
              }
              var y = $p2;
              var x = $p1;
              return Fay$$_(Fay$$_(go)(y))(Fay$$_(Fay$$_(Prelude$rem)(x))(y));
            });
          };
        };
        return Fay$$_(Fay$$_(go)(Fay$$_(Prelude$abs)(a)))(Fay$$_(Prelude$abs)(b));
      })();
    });
  };
};
var Prelude$quot = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude$error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude$quot$39$)(x))(y);
    });
  };
};
var Prelude$quot$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_int(~~(Fay$$fayToJs_int($p1)/Fay$$fayToJs_int($p2)));
    });
  };
};
var Prelude$quotRem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$list([Fay$$_(Fay$$_(Prelude$quot)(x))(y),Fay$$_(Fay$$_(Prelude$rem)(x))(y)]);
    });
  };
};
var Prelude$rem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude$error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude$rem$39$)(x))(y);
    });
  };
};
var Prelude$rem$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return Fay$$jsToFay_int(Fay$$fayToJs_int($p1) % Fay$$fayToJs_int($p2));
    });
  };
};
var Prelude$lcm = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === 0) {
        return 0;
      }
      if (Fay$$_($p1) === 0) {
        return 0;
      }
      var b = $p2;
      var a = $p1;
      return Fay$$_(Prelude$abs)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Prelude$quot)(a))(Fay$$_(Fay$$_(Prelude$gcd)(a))(b))))(b));
    });
  };
};
var Prelude$find = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Prelude$Just)(x) : Fay$$_(Fay$$_(Prelude$find)(p))(xs);
      }
      if (Fay$$_($p2) === null) {
        return Prelude$Nothing;
      }
      throw ["unhandled case in find",[$p1,$p2]];
    });
  };
};
var Prelude$filter = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$filter)(p))(xs)) : Fay$$_(Fay$$_(Prelude$filter)(p))(xs);
      }
      if (Fay$$_($p2) === null) {
        return null;
      }
      throw ["unhandled case in filter",[$p1,$p2]];
    });
  };
};
var Prelude$$_null = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    return false;
  });
};
var Prelude$map = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(f)(x)))(Fay$$_(Fay$$_(Prelude$map)(f))(xs));
      }
      throw ["unhandled case in map",[$p1,$p2]];
    });
  };
};
var Prelude$nub = function($p1){
  return new Fay$$$(function(){
    var ls = $p1;
    return Fay$$_(Fay$$_(Prelude$nub$39$)(ls))(null);
  });
};
var Prelude$nub$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p1) === null) {
        return null;
      }
      var ls = $p2;
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return Fay$$_(Fay$$_(Fay$$_(Prelude$elem)(x))(ls)) ? Fay$$_(Fay$$_(Prelude$nub$39$)(xs))(ls) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$nub$39$)(xs))(Fay$$_(Fay$$_(Fay$$cons)(x))(ls)));
      }
      throw ["unhandled case in nub'",[$p1,$p2]];
    });
  };
};
var Prelude$elem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var y = $tmp1.car;
        var ys = $tmp1.cdr;
        var x = $p1;
        return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(x))(y)))(Fay$$_(Fay$$_(Prelude$elem)(x))(ys));
      }
      if (Fay$$_($p2) === null) {
        return false;
      }
      throw ["unhandled case in elem",[$p1,$p2]];
    });
  };
};
var Prelude$notElem = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var ys = $p2;
      var x = $p1;
      return Fay$$_(Prelude$not)(Fay$$_(Fay$$_(Prelude$elem)(x))(ys));
    });
  };
};
var Prelude$sort = new Fay$$$(function(){
  return Fay$$_(Prelude$sortBy)(Prelude$compare);
});
var Prelude$sortBy = function($p1){
  return new Fay$$$(function(){
    var cmp = $p1;
    return Fay$$_(Fay$$_(Prelude$foldr)(Fay$$_(Prelude$insertBy)(cmp)))(null);
  });
};
var Prelude$insertBy = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var x = $p2;
          return Fay$$list([x]);
        }
        var ys = $p3;
        var x = $p2;
        var cmp = $p1;
        return (function($tmp1){
          if (Fay$$_($tmp1) === null) {
            return Fay$$list([x]);
          }
          var $tmp2 = Fay$$_($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var y = $tmp2.car;
            var ys$39$ = $tmp2.cdr;
            return (function($tmp2){
              if (Fay$$_($tmp2) instanceof $_Prelude$GT) {
                return Fay$$_(Fay$$_(Fay$$cons)(y))(Fay$$_(Fay$$_(Fay$$_(Prelude$insertBy)(cmp))(x))(ys$39$));
              }
              return Fay$$_(Fay$$_(Fay$$cons)(x))(ys);
            })(Fay$$_(Fay$$_(cmp)(x))(y));
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(ys);
      });
    };
  };
};
var Prelude$conc = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var ys = $p2;
      var $tmp1 = Fay$$_($p1);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$conc)(xs))(ys));
      }
      var ys = $p2;
      if (Fay$$_($p1) === null) {
        return ys;
      }
      throw ["unhandled case in conc",[$p1,$p2]];
    });
  };
};
var Prelude$concat = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$foldr)(Prelude$conc))(null);
});
var Prelude$concatMap = function($p1){
  return new Fay$$$(function(){
    var f = $p1;
    return Fay$$_(Fay$$_(Prelude$foldr)(Fay$$_(Fay$$_(Prelude$$46$)(Prelude$$43$$43$))(f)))(null);
  });
};
var Prelude$foldr = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var z = $p2;
          return z;
        }
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Fay$$_(Prelude$foldr)(f))(z))(xs));
        }
        throw ["unhandled case in foldr",[$p1,$p2,$p3]];
      });
    };
  };
};
var Prelude$foldr1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$listLen(Fay$$_($p2),1)) {
        var x = Fay$$index(0,Fay$$_($p2));
        return x;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Prelude$foldr1)(f))(xs));
      }
      if (Fay$$_($p2) === null) {
        return Fay$$_(Prelude$error)(Fay$$list("foldr1: empty list"));
      }
      throw ["unhandled case in foldr1",[$p1,$p2]];
    });
  };
};
var Prelude$foldl = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var z = $p2;
          return z;
        }
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);
        }
        throw ["unhandled case in foldl",[$p1,$p2,$p3]];
      });
    };
  };
};
var Prelude$foldl1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(f))(x))(xs);
      }
      if (Fay$$_($p2) === null) {
        return Fay$$_(Prelude$error)(Fay$$list("foldl1: empty list"));
      }
      throw ["unhandled case in foldl1",[$p1,$p2]];
    });
  };
};
var Prelude$$43$$43$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var y = $p2;
      var x = $p1;
      return Fay$$_(Fay$$_(Prelude$conc)(x))(y);
    });
  };
};
var Prelude$$33$$33$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var b = $p2;
      var a = $p1;
      return (function(){
        var go = function($p1){
          return function($p2){
            return new Fay$$$(function(){
              if (Fay$$_($p1) === null) {
                return Fay$$_(Prelude$error)(Fay$$list("(!!): index too large"));
              }
              if (Fay$$_($p2) === 0) {
                var $tmp1 = Fay$$_($p1);
                if ($tmp1 instanceof Fay$$Cons) {
                  var h = $tmp1.car;
                  return h;
                }
              }
              var n = $p2;
              var $tmp1 = Fay$$_($p1);
              if ($tmp1 instanceof Fay$$Cons) {
                var t = $tmp1.cdr;
                return Fay$$_(Fay$$_(go)(t))(Fay$$_(Fay$$_(Fay$$sub)(n))(1));
              }
              throw ["unhandled case in go",[$p1,$p2]];
            });
          };
        };
        return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0)) ? Fay$$_(Prelude$error)(Fay$$list("(!!): negative index")) : Fay$$_(Fay$$_(go)(a))(b);
      })();
    });
  };
};
var Prelude$head = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude$error)(Fay$$list("head: empty list"));
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      return h;
    }
    throw ["unhandled case in head",[$p1]];
  });
};
var Prelude$tail = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude$error)(Fay$$list("tail: empty list"));
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var t = $tmp1.cdr;
      return t;
    }
    throw ["unhandled case in tail",[$p1]];
  });
};
var Prelude$init = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude$error)(Fay$$list("init: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      var a = Fay$$index(0,Fay$$_($p1));
      return Fay$$list([a]);
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var h = $tmp1.car;
      var t = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$cons)(h))(Fay$$_(Prelude$init)(t));
    }
    throw ["unhandled case in init",[$p1]];
  });
};
var Prelude$last = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude$error)(Fay$$list("last: empty list"));
    }
    if (Fay$$listLen(Fay$$_($p1),1)) {
      var a = Fay$$index(0,Fay$$_($p1));
      return a;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var t = $tmp1.cdr;
      return Fay$$_(Prelude$last)(t);
    }
    throw ["unhandled case in last",[$p1]];
  });
};
var Prelude$iterate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$iterate)(f))(Fay$$_(f)(x)));
    });
  };
};
var Prelude$repeat = function($p1){
  return new Fay$$$(function(){
    var x = $p1;
    return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Prelude$repeat)(x));
  });
};
var Prelude$replicate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p1) === 0) {
        return null;
      }
      var x = $p2;
      var n = $p1;
      return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("replicate: negative length")) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$replicate)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(x));
    });
  };
};
var Prelude$cycle = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude$error)(Fay$$list("cycle: empty list"));
    }
    var xs = $p1;
    return (function(){
      var xs$39$ = new Fay$$$(function(){
        return Fay$$_(Fay$$_(Prelude$$43$$43$)(xs))(xs$39$);
      });
      return xs$39$;
    })();
  });
};
var Prelude$take = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p1) === 0) {
        return null;
      }
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var n = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("take: negative length")) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$take)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));
      }
      throw ["unhandled case in take",[$p1,$p2]];
    });
  };
};
var Prelude$drop = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var xs = $p2;
      if (Fay$$_($p1) === 0) {
        return xs;
      }
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var xs = $tmp1.cdr;
        var n = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("drop: negative length")) : Fay$$_(Fay$$_(Prelude$drop)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs);
      }
      throw ["unhandled case in drop",[$p1,$p2]];
    });
  };
};
var Prelude$splitAt = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var xs = $p2;
      if (Fay$$_($p1) === 0) {
        return Fay$$list([null,xs]);
      }
      if (Fay$$_($p2) === null) {
        return Fay$$list([null,null]);
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var n = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("splitAt: negative length")) : (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var a = Fay$$index(0,Fay$$_($tmp1));
            var b = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Fay$$_(Prelude$splitAt)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));
      }
      throw ["unhandled case in splitAt",[$p1,$p2]];
    });
  };
};
var Prelude$takeWhile = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$takeWhile)(p))(xs)) : null;
      }
      throw ["unhandled case in takeWhile",[$p1,$p2]];
    });
  };
};
var Prelude$dropWhile = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Prelude$dropWhile)(p))(xs) : Fay$$_(Fay$$_(Fay$$cons)(x))(xs);
      }
      throw ["unhandled case in dropWhile",[$p1,$p2]];
    });
  };
};
var Prelude$span = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return Fay$$list([null,null]);
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(p)(x)) ? (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var a = Fay$$index(0,Fay$$_($tmp1));
            var b = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Fay$$_(Prelude$span)(p))(xs)) : Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]);
      }
      throw ["unhandled case in span",[$p1,$p2]];
    });
  };
};
var Prelude$$_break = function($p1){
  return new Fay$$$(function(){
    var p = $p1;
    return Fay$$_(Prelude$span)(Fay$$_(Fay$$_(Prelude$$46$)(Prelude$not))(p));
  });
};
var Prelude$zipWith = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var b = $tmp1.car;
          var bs = $tmp1.cdr;
          var $tmp1 = Fay$$_($p2);
          if ($tmp1 instanceof Fay$$Cons) {
            var a = $tmp1.car;
            var as = $tmp1.cdr;
            var f = $p1;
            return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(a))(b)))(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(f))(as))(bs));
          }
        }
        return null;
      });
    };
  };
};
var Prelude$zipWith3 = function($p1){
  return function($p2){
    return function($p3){
      return function($p4){
        return new Fay$$$(function(){
          var $tmp1 = Fay$$_($p4);
          if ($tmp1 instanceof Fay$$Cons) {
            var c = $tmp1.car;
            var cs = $tmp1.cdr;
            var $tmp1 = Fay$$_($p3);
            if ($tmp1 instanceof Fay$$Cons) {
              var b = $tmp1.car;
              var bs = $tmp1.cdr;
              var $tmp1 = Fay$$_($p2);
              if ($tmp1 instanceof Fay$$Cons) {
                var a = $tmp1.car;
                var as = $tmp1.cdr;
                var f = $p1;
                return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$_(f)(a))(b))(c)))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith3)(f))(as))(bs))(cs));
              }
            }
          }
          return null;
        });
      };
    };
  };
};
var Prelude$zip = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var b = $tmp1.car;
        var bs = $tmp1.cdr;
        var $tmp1 = Fay$$_($p1);
        if ($tmp1 instanceof Fay$$Cons) {
          var a = $tmp1.car;
          var as = $tmp1.cdr;
          return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b])))(Fay$$_(Fay$$_(Prelude$zip)(as))(bs));
        }
      }
      return null;
    });
  };
};
var Prelude$zip3 = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var c = $tmp1.car;
          var cs = $tmp1.cdr;
          var $tmp1 = Fay$$_($p2);
          if ($tmp1 instanceof Fay$$Cons) {
            var b = $tmp1.car;
            var bs = $tmp1.cdr;
            var $tmp1 = Fay$$_($p1);
            if ($tmp1 instanceof Fay$$Cons) {
              var a = $tmp1.car;
              var as = $tmp1.cdr;
              return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b,c])))(Fay$$_(Fay$$_(Fay$$_(Prelude$zip3)(as))(bs))(cs));
            }
          }
        }
        return null;
      });
    };
  };
};
var Prelude$unzip = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),2)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Prelude$unzip)(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null]);
    }
    throw ["unhandled case in unzip",[$p1]];
  });
};
var Prelude$unzip3 = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      if (Fay$$listLen(Fay$$_($tmp1.car),3)) {
        var x = Fay$$index(0,Fay$$_($tmp1.car));
        var y = Fay$$index(1,Fay$$_($tmp1.car));
        var z = Fay$$index(2,Fay$$_($tmp1.car));
        var ps = $tmp1.cdr;
        return (function($tmp1){
          if (Fay$$listLen(Fay$$_($tmp1),3)) {
            var xs = Fay$$index(0,Fay$$_($tmp1));
            var ys = Fay$$index(1,Fay$$_($tmp1));
            var zs = Fay$$index(2,Fay$$_($tmp1));
            return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(Fay$$_(Prelude$unzip3)(ps));
      }
    }
    if (Fay$$_($p1) === null) {
      return Fay$$list([null,null,null]);
    }
    throw ["unhandled case in unzip3",[$p1]];
  });
};
var Prelude$lines = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return null;
    }
    var s = $p1;
    return (function(){
      var isLineBreak = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(c))("\r")))(Fay$$_(Fay$$_(Fay$$eq)(c))("\n"));
        });
      };
      return (function($tmp1){
        if (Fay$$listLen(Fay$$_($tmp1),2)) {
          var a = Fay$$index(0,Fay$$_($tmp1));
          if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {
            return Fay$$list([a]);
          }
          var a = Fay$$index(0,Fay$$_($tmp1));
          var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));
          if ($tmp2 instanceof Fay$$Cons) {
            var cs = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude$lines)(cs));
          }
        }
        return (function(){ throw (["unhandled case",$tmp1]); })();
      })(Fay$$_(Fay$$_(Prelude$$_break)(isLineBreak))(s));
    })();
  });
};
var Prelude$unlines = new Fay$$$(function(){
  return Fay$$_(Prelude$intercalate)(Fay$$list("\n"));
});
var Prelude$words = function($p1){
  return new Fay$$$(function(){
    var str = $p1;
    return (function(){
      var words$39$ = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) === null) {
            return null;
          }
          var s = $p1;
          return (function($tmp1){
            if (Fay$$listLen(Fay$$_($tmp1),2)) {
              var a = Fay$$index(0,Fay$$_($tmp1));
              var b = Fay$$index(1,Fay$$_($tmp1));
              return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude$words)(b));
            }
            return (function(){ throw (["unhandled case",$tmp1]); })();
          })(Fay$$_(Fay$$_(Prelude$$_break)(isSpace))(s));
        });
      };
      var isSpace = function($p1){
        return new Fay$$$(function(){
          var c = $p1;
          return Fay$$_(Fay$$_(Prelude$elem)(c))(Fay$$list(" \t\r\n\u000c\u000b"));
        });
      };
      return Fay$$_(words$39$)(Fay$$_(Fay$$_(Prelude$dropWhile)(isSpace))(str));
    })();
  });
};
var Prelude$unwords = new Fay$$$(function(){
  return Fay$$_(Prelude$intercalate)(Fay$$list(" "));
});
var Prelude$and = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return true;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$and)(x))(Fay$$_(Prelude$and)(xs));
    }
    throw ["unhandled case in and",[$p1]];
  });
};
var Prelude$or = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return false;
    }
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$_(Fay$$_(Fay$$or)(x))(Fay$$_(Prelude$or)(xs));
    }
    throw ["unhandled case in or",[$p1]];
  });
};
var Prelude$any = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return false;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude$any)(p))(xs));
      }
      throw ["unhandled case in any",[$p1,$p2]];
    });
  };
};
var Prelude$all = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return true;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var p = $p1;
        return Fay$$_(Fay$$_(Fay$$and)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude$all)(p))(xs));
      }
      throw ["unhandled case in all",[$p1,$p2]];
    });
  };
};
var Prelude$intersperse = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var sep = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$prependToAll)(sep))(xs));
      }
      throw ["unhandled case in intersperse",[$p1,$p2]];
    });
  };
};
var Prelude$prependToAll = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var sep = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(sep))(Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$prependToAll)(sep))(xs)));
      }
      throw ["unhandled case in prependToAll",[$p1,$p2]];
    });
  };
};
var Prelude$intercalate = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var xss = $p2;
      var xs = $p1;
      return Fay$$_(Prelude$concat)(Fay$$_(Fay$$_(Prelude$intersperse)(xs))(xss));
    });
  };
};
var Prelude$maximum = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude$error)(Fay$$list("maximum: empty list"));
    }
    var xs = $p1;
    return Fay$$_(Fay$$_(Prelude$foldl1)(Prelude$max))(xs);
  });
};
var Prelude$minimum = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude$error)(Fay$$list("minimum: empty list"));
    }
    var xs = $p1;
    return Fay$$_(Fay$$_(Prelude$foldl1)(Prelude$min))(xs);
  });
};
var Prelude$product = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude$error)(Fay$$list("product: empty list"));
    }
    var xs = $p1;
    return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(Fay$$mult))(1))(xs);
  });
};
var Prelude$sum = function($p1){
  return new Fay$$$(function(){
    if (Fay$$_($p1) === null) {
      return Fay$$_(Prelude$error)(Fay$$list("sum: empty list"));
    }
    var xs = $p1;
    return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(Fay$$add))(0))(xs);
  });
};
var Prelude$scanl = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var l = $p3;
        var z = $p2;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$cons)(z))((function($tmp1){
          if (Fay$$_($tmp1) === null) {
            return null;
          }
          var $tmp2 = Fay$$_($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var x = $tmp2.car;
            var xs = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$_(Prelude$scanl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);
          }
          return (function(){ throw (["unhandled case",$tmp1]); })();
        })(l));
      });
    };
  };
};
var Prelude$scanl1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return Fay$$_(Fay$$_(Fay$$_(Prelude$scanl)(f))(x))(xs);
      }
      throw ["unhandled case in scanl1",[$p1,$p2]];
    });
  };
};
var Prelude$scanr = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var z = $p2;
          return Fay$$list([z]);
        }
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var z = $p2;
          var f = $p1;
          return (function($tmp1){
            var $tmp2 = Fay$$_($tmp1);
            if ($tmp2 instanceof Fay$$Cons) {
              var h = $tmp2.car;
              var t = $tmp2.cdr;
              return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));
            }
            return Prelude$$_undefined;
          })(Fay$$_(Fay$$_(Fay$$_(Prelude$scanr)(f))(z))(xs));
        }
        throw ["unhandled case in scanr",[$p1,$p2,$p3]];
      });
    };
  };
};
var Prelude$scanr1 = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        return null;
      }
      if (Fay$$listLen(Fay$$_($p2),1)) {
        var x = Fay$$index(0,Fay$$_($p2));
        return Fay$$list([x]);
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var x = $tmp1.car;
        var xs = $tmp1.cdr;
        var f = $p1;
        return (function($tmp1){
          var $tmp2 = Fay$$_($tmp1);
          if ($tmp2 instanceof Fay$$Cons) {
            var h = $tmp2.car;
            var t = $tmp2.cdr;
            return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));
          }
          return Prelude$$_undefined;
        })(Fay$$_(Fay$$_(Prelude$scanr1)(f))(xs));
      }
      throw ["unhandled case in scanr1",[$p1,$p2]];
    });
  };
};
var Prelude$lookup = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) === null) {
        var _key = $p1;
        return Prelude$Nothing;
      }
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        if (Fay$$listLen(Fay$$_($tmp1.car),2)) {
          var x = Fay$$index(0,Fay$$_($tmp1.car));
          var y = Fay$$index(1,Fay$$_($tmp1.car));
          var xys = $tmp1.cdr;
          var key = $p1;
          return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(key))(x)) ? Fay$$_(Prelude$Just)(y) : Fay$$_(Fay$$_(Prelude$lookup)(key))(xys);
        }
      }
      throw ["unhandled case in lookup",[$p1,$p2]];
    });
  };
};
var Prelude$length = function($p1){
  return new Fay$$$(function(){
    var xs = $p1;
    return Fay$$_(Fay$$_(Prelude$length$39$)(0))(xs);
  });
};
var Prelude$length$39$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var $tmp1 = Fay$$_($p2);
      if ($tmp1 instanceof Fay$$Cons) {
        var xs = $tmp1.cdr;
        var acc = $p1;
        return Fay$$_(Fay$$_(Prelude$length$39$)(Fay$$_(Fay$$_(Fay$$add)(acc))(1)))(xs);
      }
      var acc = $p1;
      return acc;
    });
  };
};
var Prelude$reverse = function($p1){
  return new Fay$$$(function(){
    var $tmp1 = Fay$$_($p1);
    if ($tmp1 instanceof Fay$$Cons) {
      var x = $tmp1.car;
      var xs = $tmp1.cdr;
      return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$reverse)(xs)))(Fay$$list([x]));
    }
    if (Fay$$_($p1) === null) {
      return null;
    }
    throw ["unhandled case in reverse",[$p1]];
  });
};
var Prelude$print = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["automatic"],$p1))));
  });
};
var Prelude$putStrLn = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs_string($p1))));
  });
};
var Language$Fay$FFI$Nullable = function(slot1){
  return new Fay$$$(function(){
    return new $_Language$Fay$FFI$Nullable(slot1);
  });
};
var Language$Fay$FFI$Null = new Fay$$$(function(){
  return new $_Language$Fay$FFI$Null();
});
var Language$Fay$FFI$Defined = function(slot1){
  return new Fay$$$(function(){
    return new $_Language$Fay$FFI$Defined(slot1);
  });
};
var Language$Fay$FFI$Undefined = new Fay$$$(function(){
  return new $_Language$Fay$FFI$Undefined();
});
var Control$Fay$ap = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      var f = $p1;
      return Fay$$_(Fay$$_(Fay$$bind)(f))(function($p1){
        var f$39$ = $p1;
        return Fay$$_(Fay$$_(Fay$$bind)(x))(function($p1){
          var x$39$ = $p1;
          return Fay$$_(Fay$$$_return)(Fay$$_(f$39$)(x$39$));
        });
      });
    });
  };
};
var Control$Fay$mapM = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(function($p1){
    var $gen_1 = $p1;
    return Fay$$_(Fay$$_(Prelude$$46$)(Prelude$sequence))($gen_1);
  }))(Prelude$map);
});
var Control$Fay$foldM = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        if (Fay$$_($p3) === null) {
          var x = $p2;
          return Fay$$_(Fay$$$_return)(x);
        }
        var $tmp1 = Fay$$_($p3);
        if ($tmp1 instanceof Fay$$Cons) {
          var x = $tmp1.car;
          var xs = $tmp1.cdr;
          var y = $p2;
          var f = $p1;
          return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(f)(y))(x)))(function($p1){
            var z = $p1;
            return Fay$$_(Fay$$_(Fay$$_(Control$Fay$foldM)(f))(z))(xs);
          });
        }
        throw ["unhandled case in foldM",[$p1,$p2,$p3]];
      });
    };
  };
};
var Control$Fay$zipWithM = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(function($p1){
    var $gen_1 = $p1;
    return Fay$$_(Fay$$_(Prelude$$46$)(function($p1){
      var $gen_1 = $p1;
      return Fay$$_(Fay$$_(Prelude$$46$)(Prelude$sequence))($gen_1);
    }))($gen_1);
  }))(Prelude$zipWith);
});
var Control$Fay$zipWithM_ = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(function($p1){
    var $gen_1 = $p1;
    return Fay$$_(Fay$$_(Prelude$$46$)(function($p1){
      var $gen_1 = $p1;
      return Fay$$_(Fay$$_(Prelude$$46$)(Prelude$sequence_))($gen_1);
    }))($gen_1);
  }))(Prelude$zipWith);
});
var Control$Fay$replicateM = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(function($p1){
    var $gen_1 = $p1;
    return Fay$$_(Fay$$_(Prelude$$46$)(Prelude$sequence))($gen_1);
  }))(Prelude$replicate);
});
var Cinder$DSL$Content = function(slot1){
  return new Fay$$$(function(){
    return new $_Cinder$DSL$Content(slot1);
  });
};
var Cinder$DSL$Element = function(slot1){
  return new Fay$$$(function(){
    return new $_Cinder$DSL$Element(slot1);
  });
};
var Cinder$DSL$Attribute = function(slot1){
  return function(slot2){
    return new Fay$$$(function(){
      return new $_Cinder$DSL$Attribute(slot1,slot2);
    });
  };
};
var Cinder$DSL$Property = function(slot1){
  return function(slot2){
    return new Fay$$$(function(){
      return new $_Cinder$DSL$Property(slot1,slot2);
    });
  };
};
var Cinder$DSL$Complete = new Fay$$$(function(){
  return new $_Cinder$DSL$Complete();
});
var Cinder$DSL$markup = new Fay$$$(function(){
  return null;
});
var Cinder$DSL$$33$ = new Fay$$$(function(){
  return Fay$$_(Prelude$flip)(Fay$$cons);
});
var Cinder$DSL$$33$$58$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Fay$$_(Cinder$DSL$Content)(v));
    });
  };
};
var Cinder$DSL$$33$$62$ = new Fay$$$(function(){
  return Cinder$DSL$$33$;
});
var Cinder$DSL$$33$$62$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Fay$$_(Cinder$DSL$Element)(t));
    });
  };
};
var Cinder$DSL$$33$$60$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) instanceof $_Cinder$DSL$Complete) {
        var m = $p1;
        return Fay$$_(Cinder$DSL$closeAll)(m);
      }
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Cinder$DSL$Complete)))(t);
    });
  };
};
var Cinder$DSL$$33$$60$$60$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$$60$)(m))(Fay$$_(Cinder$DSL$Element)(t));
    });
  };
};
var Cinder$DSL$$33$$43$ = new Fay$$$(function(){
  return Fay$$_(Prelude$flip)(Prelude$$43$$43$);
});
var Cinder$DSL$$33$$60$$43$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$$43$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Cinder$DSL$Complete)))(t);
    });
  };
};
var Cinder$DSL$closeAll = function($p1){
  return new Fay$$$(function(){
    var m = $p1;
    return (function(){
      var n = new Fay$$$(function(){
        return Fay$$_(Fay$$_(Fay$$_(Prelude$foldr)(Cinder$DSL$nestLevel))(0))(m);
      });
      return Fay$$_(Fay$$_(Fay$$_(Fay$$gte)(n))(0)) ? Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Fay$$_(Prelude$replicate)(n))(Cinder$DSL$Complete)))(m) : Fay$$_(Prelude$error)(Fay$$list("Markup has more close elements than open"));
    })();
  });
};
var Cinder$DSL$at = new Fay$$$(function(){
  return Cinder$DSL$Attribute;
});
var Cinder$DSL$atN = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(t))(Fay$$_(Prelude$show)(v));
    });
  };
};
var Cinder$DSL$atP = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(t))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$show)(v)))(Fay$$list("%")));
    });
  };
};
var Cinder$DSL$pr = new Fay$$$(function(){
  return Cinder$DSL$Property;
});
var Cinder$DSL$prN = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$Property)(t))(Fay$$_(Prelude$show)(v));
    });
  };
};
var Cinder$DSL$co = new Fay$$$(function(){
  return Cinder$DSL$Content;
});
var Cinder$DSL$el = new Fay$$$(function(){
  return Cinder$DSL$Element;
});
var Cinder$DSL$pretty = function($p1){
  return new Fay$$$(function(){
    var m = $p1;
    return (function(){
      var rm = new Fay$$$(function(){
        return Fay$$_(Prelude$reverse)(m);
      });
      var ins = new Fay$$$(function(){
        return Fay$$_(Fay$$_(Fay$$_(Prelude$scanl)(Fay$$_(Prelude$flip)(Cinder$DSL$nestLevel)))(0))(rm);
      });
      var cat = function($p1){
        return function($p2){
          return new Fay$$$(function(){
            var x = $p2;
            var i = $p1;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Fay$$_(Prelude$replicate)(Fay$$_(Fay$$_(Fay$$mult)(i))(4)))(" ")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(go)(x)))(Fay$$list("\n")));
          });
        };
      };
      var go = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Element) {
            var x = Fay$$_($p1).slot1;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("\u003c")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$list("\u003e")));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Attribute) {
            var x = Fay$$_($p1).slot1;
            var y = Fay$$_($p1).slot2;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("=\"")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(y))(Fay$$list("\""))));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Property) {
            var x = Fay$$_($p1).slot1;
            var y = Fay$$_($p1).slot2;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("=\"")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(y))(Fay$$list("\""))));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Content) {
            var x = Fay$$_($p1).slot1;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("\"")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$list("\"")));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Complete) {
            return Fay$$list("\u003c--");
          }
          throw ["unhandled case in go",[$p1]];
        });
      };
      return Fay$$_(Fay$$_(Prelude$$36$)(Prelude$concat))(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(cat))(ins))(rm));
    })();
  });
};
var Cinder$DSL$nestLevel = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      if (Fay$$_($p1) instanceof $_Cinder$DSL$Element) {
        return Fay$$_(Fay$$_(Fay$$add)(x))(1);
      }
      var x = $p2;
      if (Fay$$_($p1) instanceof $_Cinder$DSL$Complete) {
        return Fay$$_(Fay$$_(Fay$$sub)(x))(1);
      }
      var x = $p2;
      return x;
    });
  };
};
var Cinder$DOM$root = new Fay$$$(function(){
  return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],document['documentElement']));
});
var Cinder$DOM$nodeNS = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],document['createElementNS'](Fay$$fayToJs_string($p1),Fay$$fayToJs_string($p2))));
    });
  };
};
var Cinder$DOM$content = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],document['createTextNode'](Fay$$fayToJs_string($p1))));
  });
};
var Cinder$DOM$clone = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],Fay$$fayToJs(["user","Node",[]],$p2)['cloneNode'](Fay$$fayToJs_bool($p1))));
    });
  };
};
var Cinder$DOM$setChild = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],(Fay$$fayToJs(["user","Node",[]],$p2)['appendChild'](Fay$$fayToJs(["user","Node",[]],$p1)) && null) || Fay$$fayToJs(["user","Node",[]],$p2)));
    });
  };
};
var Cinder$DOM$setParent = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],(Fay$$fayToJs(["user","Node",[]],$p1)['appendChild'](Fay$$fayToJs(["user","Node",[]],$p2)) && null) || Fay$$fayToJs(["user","Node",[]],$p2)));
    });
  };
};
var Cinder$DOM$setAttribute = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],(Fay$$fayToJs(["user","Node",[]],$p3)['setAttributeNS'](null,Fay$$fayToJs_string($p1),Fay$$fayToJs_string($p2)) && null) || Fay$$fayToJs(["user","Node",[]],$p3)));
      });
    };
  };
};
var Cinder$DOM$setProperty = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],((Fay$$fayToJs(["user","Node",[]],$p3)[Fay$$fayToJs_string($p1)] = Fay$$fayToJs_string($p2)) && null) || Fay$$fayToJs(["user","Node",[]],$p3)));
      });
    };
  };
};
var Cinder$DOM$deleteSelf = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Node",[]],$p1)['parentNode']['removeChild'](Fay$$fayToJs(["user","Node",[]],$p1))));
  });
};
var Cinder$DOM$deleteChild = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],(Fay$$fayToJs(["user","Node",[]],$p1)['removeChild'](Fay$$fayToJs(["user","Node",[]],$p2)) && null) || Fay$$fayToJs(["user","Node",[]],$p1)));
    });
  };
};
var Cinder$DOM$replace = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],Fay$$fayToJs(["user","Node",[]],$p1)['parentNode']['replaceChild'](Fay$$fayToJs(["user","Node",[]],$p2),Fay$$fayToJs(["user","Node",[]],$p1)) || Fay$$fayToJs(["user","Node",[]],$p2)));
    });
  };
};
var Cinder$DOM$byId = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],document['getElementById'](Fay$$fayToJs_string($p1))));
  });
};
var Cinder$DOM$byTag = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["list",[["user","Node",[]]]],Fay$$fayToJs(["user","Node",[]],$p2)['getElementsByTagName'](Fay$$fayToJs_string($p1))));
    });
  };
};
var Cinder$DOM$byClass = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay(["list",[["user","Node",[]]]],Fay$$fayToJs(["user","Node",[]],$p2)['getElementsByClassName'](Fay$$fayToJs_string($p1))));
    });
  };
};
var Cinder$DOM$parent = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],Fay$$fayToJs(["user","Node",[]],$p1)['parentNode']));
  });
};
var Cinder$DOM$firstChild = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],Fay$$fayToJs(["user","Node",[]],$p1)['firstChild']));
  });
};
var Cinder$DOM$lastChild = function($p1){
  return new Fay$$$(function(){
    return new Fay$$Monad(Fay$$jsToFay(["user","Node",[]],Fay$$fayToJs(["user","Node",[]],$p1)['lastChild']));
  });
};
var Cinder$DOM$attributeN = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay_double(Fay$$fayToJs(["user","Node",[]],$p2)['getAttributeNS'](null,Fay$$fayToJs_string($p1))));
    });
  };
};
var Cinder$DOM$attribute = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay_string(Fay$$fayToJs(["user","Node",[]],$p2)['getAttributeNS'](null,Fay$$fayToJs_string($p1))));
    });
  };
};
var Cinder$DOM$property = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay_string(Fay$$fayToJs(["user","Node",[]],$p2)['Fay$$fayToJs_string($p1)']));
    });
  };
};
var Cinder$DOM$propertyN = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      return new Fay$$Monad(Fay$$jsToFay_double(Fay$$fayToJs(["user","Node",[]],$p2)['Fay$$fayToJs_string($p1)']));
    });
  };
};
var Cinder$DOM$toLower = function($p1){
  return new Fay$$$(function(){
    return Fay$$jsToFay_string(String(Fay$$fayToJs_string($p1))['toLowerCase']());
  });
};
var Cinder$DOM$rgb = function($p1){
  return new Fay$$$(function(){
    if (Fay$$listLen(Fay$$_($p1),3)) {
      var r = Fay$$index(0,Fay$$_($p1));
      var g = Fay$$index(1,Fay$$_($p1));
      var b = Fay$$index(2,Fay$$_($p1));
      return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("rgb(")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$show)(r)))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list(",")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$show)(g)))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list(",")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$show)(b)))(Fay$$list(")")))))));
    }
    throw ["unhandled case in rgb",[$p1]];
  });
};
var Cinder$DOM$rgba = function($p1){
  return new Fay$$$(function(){
    if (Fay$$listLen(Fay$$_($p1),4)) {
      var r = Fay$$index(0,Fay$$_($p1));
      var g = Fay$$index(1,Fay$$_($p1));
      var b = Fay$$index(2,Fay$$_($p1));
      var a = Fay$$index(3,Fay$$_($p1));
      return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("rgba(")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$show)(r)))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list(",")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$show)(g)))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list(",")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$show)(b)))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list(",")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$show)(a)))(Fay$$list(")")))))))));
    }
    throw ["unhandled case in rgba",[$p1]];
  });
};
var Cinder$DOM$insertNS = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var n = $p3;
        var m = $p2;
        var s = $p1;
        return (function(){
          var go = function($p1){
            return function($p2){
              return new Fay$$$(function(){
                if (Fay$$_($p2) instanceof $_Cinder$DSL$Attribute) {
                  var k = Fay$$_($p2).slot1;
                  var v = Fay$$_($p2).slot2;
                  var n = $p1;
                  return Fay$$_(Fay$$_(Fay$$_(Cinder$DOM$setAttribute)(k))(v))(n);
                }
                if (Fay$$_($p2) instanceof $_Cinder$DSL$Element) {
                  var t = Fay$$_($p2).slot1;
                  var n = $p1;
                  return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Cinder$DOM$nodeNS)(s))(t)))(Fay$$_(Cinder$DOM$setParent)(n));
                }
                if (Fay$$_($p2) instanceof $_Cinder$DSL$Complete) {
                  var n = $p1;
                  return Fay$$_(Cinder$DOM$parent)(n);
                }
                if (Fay$$_($p2) instanceof $_Cinder$DSL$Property) {
                  var k = Fay$$_($p2).slot1;
                  var v = Fay$$_($p2).slot2;
                  var n = $p1;
                  return Fay$$_(Fay$$_(Fay$$_(Cinder$DOM$setProperty)(k))(v))(n);
                }
                if (Fay$$_($p2) instanceof $_Cinder$DSL$Content) {
                  var v = Fay$$_($p2).slot1;
                  var n = $p1;
                  return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Cinder$DOM$content)(v)))(Fay$$_(Cinder$DOM$setParent)(n))))(Fay$$_(Fay$$$_return)(n));
                }
                throw ["unhandled case in go",[$p1,$p2]];
              });
            };
          };
          return Fay$$_(Fay$$_(Fay$$_(Control$Fay$foldM)(go))(n))(Fay$$_(Prelude$reverse)(m));
        })();
      });
    };
  };
};
var Cinder$HTML$Attributes$abbr = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("abbr"));
});
var Cinder$HTML$Attributes$accept = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("accept"));
});
var Cinder$HTML$Attributes$acceptCharset = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("accept-charset"));
});
var Cinder$HTML$Attributes$accesskey = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("accesskey"));
});
var Cinder$HTML$Attributes$action = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("action"));
});
var Cinder$HTML$Attributes$alt = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("alt"));
});
var Cinder$HTML$Attributes$async = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("async"));
});
var Cinder$HTML$Attributes$autocomplete = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("autocomplete"));
});
var Cinder$HTML$Attributes$autofocus = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("autofocus"));
});
var Cinder$HTML$Attributes$autoplay = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("autoplay"));
});
var Cinder$HTML$Attributes$border = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("border"));
});
var Cinder$HTML$Attributes$challenge = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("challenge"));
});
var Cinder$HTML$Attributes$charset = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("charset"));
});
var Cinder$HTML$Attributes$checked = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("checked"));
});
var Cinder$HTML$Attributes$cite = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("cite"));
});
var Cinder$HTML$Attributes$cols = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("cols"));
});
var Cinder$HTML$Attributes$colspan = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("colspan"));
});
var Cinder$HTML$Attributes$command = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("command"));
});
var Cinder$HTML$Attributes$content = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("content"));
});
var Cinder$HTML$Attributes$contenteditable = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("contenteditable"));
});
var Cinder$HTML$Attributes$contextmenu = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("contextmenu"));
});
var Cinder$HTML$Attributes$controls = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("controls"));
});
var Cinder$HTML$Attributes$coords = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("coords"));
});
var Cinder$HTML$Attributes$crossorigin = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("crossorigin"));
});
var Cinder$HTML$Attributes$datetime = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("datetime"));
});
var Cinder$HTML$Attributes$defer = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("defer"));
});
var Cinder$HTML$Attributes$dir = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("dir"));
});
var Cinder$HTML$Attributes$dirname = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("dirname"));
});
var Cinder$HTML$Attributes$disabled = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("disabled"));
});
var Cinder$HTML$Attributes$draggable = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("draggable"));
});
var Cinder$HTML$Attributes$dropzone = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("dropzone"));
});
var Cinder$HTML$Attributes$enctype = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("enctype"));
});
var Cinder$HTML$Attributes$$_for = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("for"));
});
var Cinder$HTML$Attributes$form = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("form"));
});
var Cinder$HTML$Attributes$formaction = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formaction"));
});
var Cinder$HTML$Attributes$formenctype = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formenctype"));
});
var Cinder$HTML$Attributes$formmethod = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formmethod"));
});
var Cinder$HTML$Attributes$formnovalidate = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formnovalidate"));
});
var Cinder$HTML$Attributes$formtarget = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formtarget"));
});
var Cinder$HTML$Attributes$headers = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("headers"));
});
var Cinder$HTML$Attributes$height = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("height"));
});
var Cinder$HTML$Attributes$hidden = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("hidden"));
});
var Cinder$HTML$Attributes$high = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("high"));
});
var Cinder$HTML$Attributes$href = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("href"));
});
var Cinder$HTML$Attributes$hreflang = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("hreflang"));
});
var Cinder$HTML$Attributes$httpEquiv = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("http-equiv"));
});
var Cinder$HTML$Attributes$icon = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("icon"));
});
var Cinder$HTML$Attributes$id = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("id"));
});
var Cinder$HTML$Attributes$ismap = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("ismap"));
});
var Cinder$HTML$Attributes$keytype = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("keytype"));
});
var Cinder$HTML$Attributes$kind = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("kind"));
});
var Cinder$HTML$Attributes$label = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("label"));
});
var Cinder$HTML$Attributes$lang = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("lang"));
});
var Cinder$HTML$Attributes$list = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("list"));
});
var Cinder$HTML$Attributes$loop = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("loop"));
});
var Cinder$HTML$Attributes$low = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("low"));
});
var Cinder$HTML$Attributes$manifest = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("manifest"));
});
var Cinder$HTML$Attributes$max = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("max"));
});
var Cinder$HTML$Attributes$maxlength = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("maxlength"));
});
var Cinder$HTML$Attributes$media = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("media"));
});
var Cinder$HTML$Attributes$mediagroup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("mediagroup"));
});
var Cinder$HTML$Attributes$method = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("method"));
});
var Cinder$HTML$Attributes$min = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("min"));
});
var Cinder$HTML$Attributes$multiple = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("multiple"));
});
var Cinder$HTML$Attributes$muted = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("muted"));
});
var Cinder$HTML$Attributes$name = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("name"));
});
var Cinder$HTML$Attributes$novalidate = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("novalidate"));
});
var Cinder$HTML$Attributes$open = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("open"));
});
var Cinder$HTML$Attributes$optimum = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("optimum"));
});
var Cinder$HTML$Attributes$pattern = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("pattern"));
});
var Cinder$HTML$Attributes$placeholder = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("placeholder"));
});
var Cinder$HTML$Attributes$poster = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("poster"));
});
var Cinder$HTML$Attributes$preload = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("preload"));
});
var Cinder$HTML$Attributes$radiogroup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("radiogroup"));
});
var Cinder$HTML$Attributes$readonly = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("readonly"));
});
var Cinder$HTML$Attributes$rel = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("rel"));
});
var Cinder$HTML$Attributes$required = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("required"));
});
var Cinder$HTML$Attributes$reversed = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("reversed"));
});
var Cinder$HTML$Attributes$rows = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("rows"));
});
var Cinder$HTML$Attributes$rowspan = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("rowspan"));
});
var Cinder$HTML$Attributes$sandbox = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("sandbox"));
});
var Cinder$HTML$Attributes$scope = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("scope"));
});
var Cinder$HTML$Attributes$scoped = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("scoped"));
});
var Cinder$HTML$Attributes$seamless = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("seamless"));
});
var Cinder$HTML$Attributes$selected = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("selected"));
});
var Cinder$HTML$Attributes$shape = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("shape"));
});
var Cinder$HTML$Attributes$size = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("size"));
});
var Cinder$HTML$Attributes$sizes = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("sizes"));
});
var Cinder$HTML$Attributes$span = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("span"));
});
var Cinder$HTML$Attributes$spellcheck = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("spellcheck"));
});
var Cinder$HTML$Attributes$src = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("src"));
});
var Cinder$HTML$Attributes$srcdoc = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("srcdoc"));
});
var Cinder$HTML$Attributes$srclang = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("srclang"));
});
var Cinder$HTML$Attributes$start = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("start"));
});
var Cinder$HTML$Attributes$step = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("step"));
});
var Cinder$HTML$Attributes$style = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("style"));
});
var Cinder$HTML$Attributes$tabindex = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("tabindex"));
});
var Cinder$HTML$Attributes$target = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("target"));
});
var Cinder$HTML$Attributes$title = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("title"));
});
var Cinder$HTML$Attributes$translate = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("translate"));
});
var Cinder$HTML$Attributes$typemustmatch = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("typemustmatch"));
});
var Cinder$HTML$Attributes$usemap = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("usemap"));
});
var Cinder$HTML$Attributes$value = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("value"));
});
var Cinder$HTML$Attributes$width = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("width"));
});
var Cinder$HTML$Attributes$wrap = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("wrap"));
});
var Cinder$HTML$Elements$a = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("a"));
});
var Cinder$HTML$Elements$abbr = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("abbr"));
});
var Cinder$HTML$Elements$address = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("address"));
});
var Cinder$HTML$Elements$area = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("area"));
});
var Cinder$HTML$Elements$article = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("article"));
});
var Cinder$HTML$Elements$aside = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("aside"));
});
var Cinder$HTML$Elements$audio = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("audio"));
});
var Cinder$HTML$Elements$b = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("b"));
});
var Cinder$HTML$Elements$base = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("base"));
});
var Cinder$HTML$Elements$bdi = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("bdi"));
});
var Cinder$HTML$Elements$bdo = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("bdo"));
});
var Cinder$HTML$Elements$blockquote = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("blockquote"));
});
var Cinder$HTML$Elements$body = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("body"));
});
var Cinder$HTML$Elements$br = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("br"));
});
var Cinder$HTML$Elements$button = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("button"));
});
var Cinder$HTML$Elements$canvas = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("canvas"));
});
var Cinder$HTML$Elements$caption = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("caption"));
});
var Cinder$HTML$Elements$cite = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("cite"));
});
var Cinder$HTML$Elements$code = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("code"));
});
var Cinder$HTML$Elements$col = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("col"));
});
var Cinder$HTML$Elements$colgroup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("colgroup"));
});
var Cinder$HTML$Elements$command = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("command"));
});
var Cinder$HTML$Elements$datalist = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("datalist"));
});
var Cinder$HTML$Elements$dd = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("dd"));
});
var Cinder$HTML$Elements$del = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("del"));
});
var Cinder$HTML$Elements$details = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("details"));
});
var Cinder$HTML$Elements$dfn = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("dfn"));
});
var Cinder$HTML$Elements$dialog = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("dialog"));
});
var Cinder$HTML$Elements$div = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("div"));
});
var Cinder$HTML$Elements$dl = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("dl"));
});
var Cinder$HTML$Elements$dt = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("dt"));
});
var Cinder$HTML$Elements$em = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("em"));
});
var Cinder$HTML$Elements$embed = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("embed"));
});
var Cinder$HTML$Elements$fieldset = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("fieldset"));
});
var Cinder$HTML$Elements$figcaption = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("figcaption"));
});
var Cinder$HTML$Elements$figure = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("figure"));
});
var Cinder$HTML$Elements$footer = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("footer"));
});
var Cinder$HTML$Elements$form = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("form"));
});
var Cinder$HTML$Elements$h1 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h1"));
});
var Cinder$HTML$Elements$h2 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h2"));
});
var Cinder$HTML$Elements$h3 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h3"));
});
var Cinder$HTML$Elements$h4 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h4"));
});
var Cinder$HTML$Elements$h5 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h5"));
});
var Cinder$HTML$Elements$h6 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h6"));
});
var Cinder$HTML$Elements$head = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("head"));
});
var Cinder$HTML$Elements$header = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("header"));
});
var Cinder$HTML$Elements$hgroup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("hgroup"));
});
var Cinder$HTML$Elements$hr = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("hr"));
});
var Cinder$HTML$Elements$html = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("html"));
});
var Cinder$HTML$Elements$i = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("i"));
});
var Cinder$HTML$Elements$iframe = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("iframe"));
});
var Cinder$HTML$Elements$img = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("img"));
});
var Cinder$HTML$Elements$input = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("input"));
});
var Cinder$HTML$Elements$ins = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("ins"));
});
var Cinder$HTML$Elements$kbd = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("kbd"));
});
var Cinder$HTML$Elements$keygen = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("keygen"));
});
var Cinder$HTML$Elements$label = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("label"));
});
var Cinder$HTML$Elements$legend = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("legend"));
});
var Cinder$HTML$Elements$li = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("li"));
});
var Cinder$HTML$Elements$link = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("link"));
});
var Cinder$HTML$Elements$map = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("map"));
});
var Cinder$HTML$Elements$mark = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("mark"));
});
var Cinder$HTML$Elements$menu = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("menu"));
});
var Cinder$HTML$Elements$meta = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("meta"));
});
var Cinder$HTML$Elements$meter = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("meter"));
});
var Cinder$HTML$Elements$nav = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("nav"));
});
var Cinder$HTML$Elements$noscript = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("noscript"));
});
var Cinder$HTML$Elements$object = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("object"));
});
var Cinder$HTML$Elements$ol = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("ol"));
});
var Cinder$HTML$Elements$optgroup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("optgroup"));
});
var Cinder$HTML$Elements$option = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("option"));
});
var Cinder$HTML$Elements$output = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("output"));
});
var Cinder$HTML$Elements$p = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("p"));
});
var Cinder$HTML$Elements$param = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("param"));
});
var Cinder$HTML$Elements$pre = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("pre"));
});
var Cinder$HTML$Elements$progress = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("progress"));
});
var Cinder$HTML$Elements$q = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("q"));
});
var Cinder$HTML$Elements$rp = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("rp"));
});
var Cinder$HTML$Elements$rt = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("rt"));
});
var Cinder$HTML$Elements$ruby = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("ruby"));
});
var Cinder$HTML$Elements$s = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("s"));
});
var Cinder$HTML$Elements$samp = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("samp"));
});
var Cinder$HTML$Elements$script = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("script"));
});
var Cinder$HTML$Elements$section = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("section"));
});
var Cinder$HTML$Elements$select = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("select"));
});
var Cinder$HTML$Elements$small = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("small"));
});
var Cinder$HTML$Elements$source = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("source"));
});
var Cinder$HTML$Elements$span = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("span"));
});
var Cinder$HTML$Elements$strong = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("strong"));
});
var Cinder$HTML$Elements$style = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("style"));
});
var Cinder$HTML$Elements$sub = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("sub"));
});
var Cinder$HTML$Elements$summary = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("summary"));
});
var Cinder$HTML$Elements$sup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("sup"));
});
var Cinder$HTML$Elements$table = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("table"));
});
var Cinder$HTML$Elements$tbody = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("tbody"));
});
var Cinder$HTML$Elements$td = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("td"));
});
var Cinder$HTML$Elements$textarea = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("textarea"));
});
var Cinder$HTML$Elements$tfoot = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("tfoot"));
});
var Cinder$HTML$Elements$th = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("th"));
});
var Cinder$HTML$Elements$thead = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("thead"));
});
var Cinder$HTML$Elements$time = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("time"));
});
var Cinder$HTML$Elements$title = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("title"));
});
var Cinder$HTML$Elements$tr = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("tr"));
});
var Cinder$HTML$Elements$track = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("track"));
});
var Cinder$HTML$Elements$u = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("u"));
});
var Cinder$HTML$Elements$ul = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("ul"));
});
var Cinder$HTML$Elements$$_var = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("var"));
});
var Cinder$HTML$Elements$video = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("video"));
});
var Cinder$HTML$Elements$wbr = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("wbr"));
});
var Cinder$HTML$xmlns = new Fay$$$(function(){
  return Fay$$list("http://www.w3.org/1999/xhtml");
});
var Cinder$HTML$imgS = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(function($p1){
    var $gen_1 = $p1;
    return Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(Cinder$DSL$markup))(Cinder$HTML$Elements$img)))($gen_1);
  }))(Cinder$HTML$Attributes$src);
});
var Cinder$HTML$imgSA = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var a = $p2;
      var s = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Cinder$HTML$imgS)(s)))(Fay$$_(Cinder$HTML$Attributes$alt)(a));
    });
  };
};
var Cinder$HTML$eX = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var c = $p2;
      var e = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$$58$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(Cinder$DSL$markup))(e)))(c);
    });
  };
};
var Cinder$HTML$eCX = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        var c = $p3;
        var cl = $p2;
        var e = $p1;
        return Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Fay$$_(Cinder$HTML$eX)(e))(c)))(Fay$$_(Cinder$HTML$classA)(cl));
      });
    };
  };
};
var Cinder$HTML$dX = new Fay$$$(function(){
  return Fay$$_(Cinder$HTML$eX)(Cinder$HTML$Elements$div);
});
var Cinder$HTML$dCX = new Fay$$$(function(){
  return Fay$$_(Cinder$HTML$eCX)(Cinder$HTML$Elements$div);
});
var Cinder$HTML$pX = new Fay$$$(function(){
  return Fay$$_(Cinder$HTML$eX)(Cinder$HTML$Elements$p);
});
var Cinder$HTML$pCX = new Fay$$$(function(){
  return Fay$$_(Cinder$HTML$eCX)(Cinder$HTML$Elements$p);
});
var Cinder$HTML$iX = new Fay$$$(function(){
  return Fay$$_(Cinder$HTML$eX)(Cinder$HTML$Elements$i);
});
var Cinder$HTML$bX = new Fay$$$(function(){
  return Fay$$_(Cinder$HTML$eX)(Cinder$HTML$Elements$b);
});
var Cinder$HTML$strongX = new Fay$$$(function(){
  return Fay$$_(Cinder$HTML$eX)(Cinder$HTML$Elements$strong);
});
var Cinder$HTML$insert = new Fay$$$(function(){
  return Fay$$_(Cinder$DOM$insertNS)(Cinder$HTML$xmlns);
});
var Cinder$HTML$node = new Fay$$$(function(){
  return Fay$$_(Cinder$DOM$nodeNS)(Cinder$HTML$xmlns);
});
var Cinder$HTML$classA = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("class"));
});
var Cinder$HTML$typeA = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("type"));
});
var Cinder$HTML$dataA = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("data"));
});
var Cinder$HTML$defaultA = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("default"));
});
var Cinder$HTML$inner = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Property)(Fay$$list("innerHTML"));
});
var Cinder$HTML$asyncB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("async")))(Fay$$list(""));
});
var Cinder$HTML$autofocusB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("autofocus")))(Fay$$list(""));
});
var Cinder$HTML$autoplayB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("autoplay")))(Fay$$list(""));
});
var Cinder$HTML$checkedB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("checked")))(Fay$$list(""));
});
var Cinder$HTML$controlsB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("controls")))(Fay$$list(""));
});
var Cinder$HTML$defaultB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("default")))(Fay$$list(""));
});
var Cinder$HTML$deferB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("defer")))(Fay$$list(""));
});
var Cinder$HTML$disabledB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("disabled")))(Fay$$list(""));
});
var Cinder$HTML$formnovalidateB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formnovalidate")))(Fay$$list(""));
});
var Cinder$HTML$hiddenB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("hidden")))(Fay$$list(""));
});
var Cinder$HTML$ismapB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("ismap")))(Fay$$list(""));
});
var Cinder$HTML$loopB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("loop")))(Fay$$list(""));
});
var Cinder$HTML$multipleB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("multiple")))(Fay$$list(""));
});
var Cinder$HTML$mutedB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("muted")))(Fay$$list(""));
});
var Cinder$HTML$novalidateB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("novalidate")))(Fay$$list(""));
});
var Cinder$HTML$openB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("open")))(Fay$$list(""));
});
var Cinder$HTML$readonlyB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("readonly")))(Fay$$list(""));
});
var Cinder$HTML$requiredB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("required")))(Fay$$list(""));
});
var Cinder$HTML$reversedB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("reversed")))(Fay$$list(""));
});
var Cinder$HTML$scopedB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("scoped")))(Fay$$list(""));
});
var Cinder$HTML$seamlessB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("seamless")))(Fay$$list(""));
});
var Cinder$HTML$selectedB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("selected")))(Fay$$list(""));
});
var Cinder$HTML$typemustmatchB = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("typemustmatch")))(Fay$$list(""));
});
var Cinder$HTML$colsN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("cols"))))(Prelude$show);
});
var Cinder$HTML$colspanN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("colspan"))))(Prelude$show);
});
var Cinder$HTML$coordsN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("coords"))))(Prelude$show);
});
var Cinder$HTML$datetimeN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("datetime"))))(Prelude$show);
});
var Cinder$HTML$heightN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("height"))))(Prelude$show);
});
var Cinder$HTML$highN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("high"))))(Prelude$show);
});
var Cinder$HTML$lowN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("low"))))(Prelude$show);
});
var Cinder$HTML$maxN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("max"))))(Prelude$show);
});
var Cinder$HTML$maxlengthN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("maxlength"))))(Prelude$show);
});
var Cinder$HTML$minN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("min"))))(Prelude$show);
});
var Cinder$HTML$optimumN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("optimum"))))(Prelude$show);
});
var Cinder$HTML$rowsN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("rows"))))(Prelude$show);
});
var Cinder$HTML$rowspanN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("rowspan"))))(Prelude$show);
});
var Cinder$HTML$sizeN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("size"))))(Prelude$show);
});
var Cinder$HTML$spanN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("span"))))(Prelude$show);
});
var Cinder$HTML$startN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("start"))))(Prelude$show);
});
var Cinder$HTML$stepN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("step"))))(Prelude$show);
});
var Cinder$HTML$tabindexN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("tabindex"))))(Prelude$show);
});
var Cinder$HTML$valueN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("value"))))(Prelude$show);
});
var Cinder$HTML$widthN = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Prelude$$46$)(Fay$$_(Cinder$DSL$Attribute)(Fay$$list("width"))))(Prelude$show);
});
var Cinder$DSL$Content = function(slot1){
  return new Fay$$$(function(){
    return new $_Cinder$DSL$Content(slot1);
  });
};
var Cinder$DSL$Element = function(slot1){
  return new Fay$$$(function(){
    return new $_Cinder$DSL$Element(slot1);
  });
};
var Cinder$DSL$Attribute = function(slot1){
  return function(slot2){
    return new Fay$$$(function(){
      return new $_Cinder$DSL$Attribute(slot1,slot2);
    });
  };
};
var Cinder$DSL$Property = function(slot1){
  return function(slot2){
    return new Fay$$$(function(){
      return new $_Cinder$DSL$Property(slot1,slot2);
    });
  };
};
var Cinder$DSL$Complete = new Fay$$$(function(){
  return new $_Cinder$DSL$Complete();
});
var Cinder$DSL$markup = new Fay$$$(function(){
  return null;
});
var Cinder$DSL$$33$ = new Fay$$$(function(){
  return Fay$$_(Prelude$flip)(Fay$$cons);
});
var Cinder$DSL$$33$$58$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Fay$$_(Cinder$DSL$Content)(v));
    });
  };
};
var Cinder$DSL$$33$$62$ = new Fay$$$(function(){
  return Cinder$DSL$$33$;
});
var Cinder$DSL$$33$$62$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Fay$$_(Cinder$DSL$Element)(t));
    });
  };
};
var Cinder$DSL$$33$$60$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) instanceof $_Cinder$DSL$Complete) {
        var m = $p1;
        return Fay$$_(Cinder$DSL$closeAll)(m);
      }
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Cinder$DSL$Complete)))(t);
    });
  };
};
var Cinder$DSL$$33$$60$$60$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$$60$)(m))(Fay$$_(Cinder$DSL$Element)(t));
    });
  };
};
var Cinder$DSL$$33$$43$ = new Fay$$$(function(){
  return Fay$$_(Prelude$flip)(Prelude$$43$$43$);
});
var Cinder$DSL$$33$$60$$43$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$$43$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Cinder$DSL$Complete)))(t);
    });
  };
};
var Cinder$DSL$closeAll = function($p1){
  return new Fay$$$(function(){
    var m = $p1;
    return (function(){
      var n = new Fay$$$(function(){
        return Fay$$_(Fay$$_(Fay$$_(Prelude$foldr)(Cinder$DSL$nestLevel))(0))(m);
      });
      return Fay$$_(Fay$$_(Fay$$_(Fay$$gte)(n))(0)) ? Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Fay$$_(Prelude$replicate)(n))(Cinder$DSL$Complete)))(m) : Fay$$_(Prelude$error)(Fay$$list("Markup has more close elements than open"));
    })();
  });
};
var Cinder$DSL$at = new Fay$$$(function(){
  return Cinder$DSL$Attribute;
});
var Cinder$DSL$atN = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(t))(Fay$$_(Prelude$show)(v));
    });
  };
};
var Cinder$DSL$atP = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(t))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$show)(v)))(Fay$$list("%")));
    });
  };
};
var Cinder$DSL$pr = new Fay$$$(function(){
  return Cinder$DSL$Property;
});
var Cinder$DSL$prN = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$Property)(t))(Fay$$_(Prelude$show)(v));
    });
  };
};
var Cinder$DSL$co = new Fay$$$(function(){
  return Cinder$DSL$Content;
});
var Cinder$DSL$el = new Fay$$$(function(){
  return Cinder$DSL$Element;
});
var Cinder$DSL$pretty = function($p1){
  return new Fay$$$(function(){
    var m = $p1;
    return (function(){
      var rm = new Fay$$$(function(){
        return Fay$$_(Prelude$reverse)(m);
      });
      var ins = new Fay$$$(function(){
        return Fay$$_(Fay$$_(Fay$$_(Prelude$scanl)(Fay$$_(Prelude$flip)(Cinder$DSL$nestLevel)))(0))(rm);
      });
      var cat = function($p1){
        return function($p2){
          return new Fay$$$(function(){
            var x = $p2;
            var i = $p1;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Fay$$_(Prelude$replicate)(Fay$$_(Fay$$_(Fay$$mult)(i))(4)))(" ")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(go)(x)))(Fay$$list("\n")));
          });
        };
      };
      var go = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Element) {
            var x = Fay$$_($p1).slot1;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("\u003c")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$list("\u003e")));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Attribute) {
            var x = Fay$$_($p1).slot1;
            var y = Fay$$_($p1).slot2;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("=\"")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(y))(Fay$$list("\""))));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Property) {
            var x = Fay$$_($p1).slot1;
            var y = Fay$$_($p1).slot2;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("=\"")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(y))(Fay$$list("\""))));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Content) {
            var x = Fay$$_($p1).slot1;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("\"")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$list("\"")));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Complete) {
            return Fay$$list("\u003c--");
          }
          throw ["unhandled case in go",[$p1]];
        });
      };
      return Fay$$_(Fay$$_(Prelude$$36$)(Prelude$concat))(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(cat))(ins))(rm));
    })();
  });
};
var Cinder$DSL$nestLevel = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      if (Fay$$_($p1) instanceof $_Cinder$DSL$Element) {
        return Fay$$_(Fay$$_(Fay$$add)(x))(1);
      }
      var x = $p2;
      if (Fay$$_($p1) instanceof $_Cinder$DSL$Complete) {
        return Fay$$_(Fay$$_(Fay$$sub)(x))(1);
      }
      var x = $p2;
      return x;
    });
  };
};
var Cinder$HTML$Elements$a = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("a"));
});
var Cinder$HTML$Elements$abbr = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("abbr"));
});
var Cinder$HTML$Elements$address = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("address"));
});
var Cinder$HTML$Elements$area = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("area"));
});
var Cinder$HTML$Elements$article = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("article"));
});
var Cinder$HTML$Elements$aside = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("aside"));
});
var Cinder$HTML$Elements$audio = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("audio"));
});
var Cinder$HTML$Elements$b = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("b"));
});
var Cinder$HTML$Elements$base = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("base"));
});
var Cinder$HTML$Elements$bdi = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("bdi"));
});
var Cinder$HTML$Elements$bdo = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("bdo"));
});
var Cinder$HTML$Elements$blockquote = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("blockquote"));
});
var Cinder$HTML$Elements$body = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("body"));
});
var Cinder$HTML$Elements$br = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("br"));
});
var Cinder$HTML$Elements$button = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("button"));
});
var Cinder$HTML$Elements$canvas = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("canvas"));
});
var Cinder$HTML$Elements$caption = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("caption"));
});
var Cinder$HTML$Elements$cite = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("cite"));
});
var Cinder$HTML$Elements$code = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("code"));
});
var Cinder$HTML$Elements$col = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("col"));
});
var Cinder$HTML$Elements$colgroup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("colgroup"));
});
var Cinder$HTML$Elements$command = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("command"));
});
var Cinder$HTML$Elements$datalist = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("datalist"));
});
var Cinder$HTML$Elements$dd = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("dd"));
});
var Cinder$HTML$Elements$del = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("del"));
});
var Cinder$HTML$Elements$details = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("details"));
});
var Cinder$HTML$Elements$dfn = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("dfn"));
});
var Cinder$HTML$Elements$dialog = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("dialog"));
});
var Cinder$HTML$Elements$div = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("div"));
});
var Cinder$HTML$Elements$dl = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("dl"));
});
var Cinder$HTML$Elements$dt = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("dt"));
});
var Cinder$HTML$Elements$em = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("em"));
});
var Cinder$HTML$Elements$embed = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("embed"));
});
var Cinder$HTML$Elements$fieldset = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("fieldset"));
});
var Cinder$HTML$Elements$figcaption = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("figcaption"));
});
var Cinder$HTML$Elements$figure = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("figure"));
});
var Cinder$HTML$Elements$footer = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("footer"));
});
var Cinder$HTML$Elements$form = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("form"));
});
var Cinder$HTML$Elements$h1 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h1"));
});
var Cinder$HTML$Elements$h2 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h2"));
});
var Cinder$HTML$Elements$h3 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h3"));
});
var Cinder$HTML$Elements$h4 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h4"));
});
var Cinder$HTML$Elements$h5 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h5"));
});
var Cinder$HTML$Elements$h6 = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("h6"));
});
var Cinder$HTML$Elements$head = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("head"));
});
var Cinder$HTML$Elements$header = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("header"));
});
var Cinder$HTML$Elements$hgroup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("hgroup"));
});
var Cinder$HTML$Elements$hr = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("hr"));
});
var Cinder$HTML$Elements$html = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("html"));
});
var Cinder$HTML$Elements$i = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("i"));
});
var Cinder$HTML$Elements$iframe = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("iframe"));
});
var Cinder$HTML$Elements$img = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("img"));
});
var Cinder$HTML$Elements$input = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("input"));
});
var Cinder$HTML$Elements$ins = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("ins"));
});
var Cinder$HTML$Elements$kbd = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("kbd"));
});
var Cinder$HTML$Elements$keygen = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("keygen"));
});
var Cinder$HTML$Elements$label = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("label"));
});
var Cinder$HTML$Elements$legend = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("legend"));
});
var Cinder$HTML$Elements$li = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("li"));
});
var Cinder$HTML$Elements$link = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("link"));
});
var Cinder$HTML$Elements$map = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("map"));
});
var Cinder$HTML$Elements$mark = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("mark"));
});
var Cinder$HTML$Elements$menu = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("menu"));
});
var Cinder$HTML$Elements$meta = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("meta"));
});
var Cinder$HTML$Elements$meter = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("meter"));
});
var Cinder$HTML$Elements$nav = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("nav"));
});
var Cinder$HTML$Elements$noscript = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("noscript"));
});
var Cinder$HTML$Elements$object = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("object"));
});
var Cinder$HTML$Elements$ol = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("ol"));
});
var Cinder$HTML$Elements$optgroup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("optgroup"));
});
var Cinder$HTML$Elements$option = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("option"));
});
var Cinder$HTML$Elements$output = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("output"));
});
var Cinder$HTML$Elements$p = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("p"));
});
var Cinder$HTML$Elements$param = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("param"));
});
var Cinder$HTML$Elements$pre = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("pre"));
});
var Cinder$HTML$Elements$progress = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("progress"));
});
var Cinder$HTML$Elements$q = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("q"));
});
var Cinder$HTML$Elements$rp = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("rp"));
});
var Cinder$HTML$Elements$rt = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("rt"));
});
var Cinder$HTML$Elements$ruby = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("ruby"));
});
var Cinder$HTML$Elements$s = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("s"));
});
var Cinder$HTML$Elements$samp = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("samp"));
});
var Cinder$HTML$Elements$script = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("script"));
});
var Cinder$HTML$Elements$section = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("section"));
});
var Cinder$HTML$Elements$select = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("select"));
});
var Cinder$HTML$Elements$small = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("small"));
});
var Cinder$HTML$Elements$source = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("source"));
});
var Cinder$HTML$Elements$span = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("span"));
});
var Cinder$HTML$Elements$strong = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("strong"));
});
var Cinder$HTML$Elements$style = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("style"));
});
var Cinder$HTML$Elements$sub = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("sub"));
});
var Cinder$HTML$Elements$summary = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("summary"));
});
var Cinder$HTML$Elements$sup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("sup"));
});
var Cinder$HTML$Elements$table = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("table"));
});
var Cinder$HTML$Elements$tbody = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("tbody"));
});
var Cinder$HTML$Elements$td = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("td"));
});
var Cinder$HTML$Elements$textarea = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("textarea"));
});
var Cinder$HTML$Elements$tfoot = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("tfoot"));
});
var Cinder$HTML$Elements$th = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("th"));
});
var Cinder$HTML$Elements$thead = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("thead"));
});
var Cinder$HTML$Elements$time = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("time"));
});
var Cinder$HTML$Elements$title = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("title"));
});
var Cinder$HTML$Elements$tr = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("tr"));
});
var Cinder$HTML$Elements$track = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("track"));
});
var Cinder$HTML$Elements$u = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("u"));
});
var Cinder$HTML$Elements$ul = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("ul"));
});
var Cinder$HTML$Elements$$_var = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("var"));
});
var Cinder$HTML$Elements$video = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("video"));
});
var Cinder$HTML$Elements$wbr = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Element)(Fay$$list("wbr"));
});
var Cinder$DSL$Content = function(slot1){
  return new Fay$$$(function(){
    return new $_Cinder$DSL$Content(slot1);
  });
};
var Cinder$DSL$Element = function(slot1){
  return new Fay$$$(function(){
    return new $_Cinder$DSL$Element(slot1);
  });
};
var Cinder$DSL$Attribute = function(slot1){
  return function(slot2){
    return new Fay$$$(function(){
      return new $_Cinder$DSL$Attribute(slot1,slot2);
    });
  };
};
var Cinder$DSL$Property = function(slot1){
  return function(slot2){
    return new Fay$$$(function(){
      return new $_Cinder$DSL$Property(slot1,slot2);
    });
  };
};
var Cinder$DSL$Complete = new Fay$$$(function(){
  return new $_Cinder$DSL$Complete();
});
var Cinder$DSL$markup = new Fay$$$(function(){
  return null;
});
var Cinder$DSL$$33$ = new Fay$$$(function(){
  return Fay$$_(Prelude$flip)(Fay$$cons);
});
var Cinder$DSL$$33$$58$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Fay$$_(Cinder$DSL$Content)(v));
    });
  };
};
var Cinder$DSL$$33$$62$ = new Fay$$$(function(){
  return Cinder$DSL$$33$;
});
var Cinder$DSL$$33$$62$$62$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Fay$$_(Cinder$DSL$Element)(t));
    });
  };
};
var Cinder$DSL$$33$$60$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      if (Fay$$_($p2) instanceof $_Cinder$DSL$Complete) {
        var m = $p1;
        return Fay$$_(Cinder$DSL$closeAll)(m);
      }
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Cinder$DSL$Complete)))(t);
    });
  };
};
var Cinder$DSL$$33$$60$$60$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$$60$)(m))(Fay$$_(Cinder$DSL$Element)(t));
    });
  };
};
var Cinder$DSL$$33$$43$ = new Fay$$$(function(){
  return Fay$$_(Prelude$flip)(Prelude$$43$$43$);
});
var Cinder$DSL$$33$$60$$43$ = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var t = $p2;
      var m = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$$43$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(m))(Cinder$DSL$Complete)))(t);
    });
  };
};
var Cinder$DSL$closeAll = function($p1){
  return new Fay$$$(function(){
    var m = $p1;
    return (function(){
      var n = new Fay$$$(function(){
        return Fay$$_(Fay$$_(Fay$$_(Prelude$foldr)(Cinder$DSL$nestLevel))(0))(m);
      });
      return Fay$$_(Fay$$_(Fay$$_(Fay$$gte)(n))(0)) ? Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Fay$$_(Prelude$replicate)(n))(Cinder$DSL$Complete)))(m) : Fay$$_(Prelude$error)(Fay$$list("Markup has more close elements than open"));
    })();
  });
};
var Cinder$DSL$at = new Fay$$$(function(){
  return Cinder$DSL$Attribute;
});
var Cinder$DSL$atN = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(t))(Fay$$_(Prelude$show)(v));
    });
  };
};
var Cinder$DSL$atP = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$Attribute)(t))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$show)(v)))(Fay$$list("%")));
    });
  };
};
var Cinder$DSL$pr = new Fay$$$(function(){
  return Cinder$DSL$Property;
});
var Cinder$DSL$prN = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var v = $p2;
      var t = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$Property)(t))(Fay$$_(Prelude$show)(v));
    });
  };
};
var Cinder$DSL$co = new Fay$$$(function(){
  return Cinder$DSL$Content;
});
var Cinder$DSL$el = new Fay$$$(function(){
  return Cinder$DSL$Element;
});
var Cinder$DSL$pretty = function($p1){
  return new Fay$$$(function(){
    var m = $p1;
    return (function(){
      var rm = new Fay$$$(function(){
        return Fay$$_(Prelude$reverse)(m);
      });
      var ins = new Fay$$$(function(){
        return Fay$$_(Fay$$_(Fay$$_(Prelude$scanl)(Fay$$_(Prelude$flip)(Cinder$DSL$nestLevel)))(0))(rm);
      });
      var cat = function($p1){
        return function($p2){
          return new Fay$$$(function(){
            var x = $p2;
            var i = $p1;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Fay$$_(Prelude$replicate)(Fay$$_(Fay$$_(Fay$$mult)(i))(4)))(" ")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(go)(x)))(Fay$$list("\n")));
          });
        };
      };
      var go = function($p1){
        return new Fay$$$(function(){
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Element) {
            var x = Fay$$_($p1).slot1;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("\u003c")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$list("\u003e")));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Attribute) {
            var x = Fay$$_($p1).slot1;
            var y = Fay$$_($p1).slot2;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("=\"")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(y))(Fay$$list("\""))));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Property) {
            var x = Fay$$_($p1).slot1;
            var y = Fay$$_($p1).slot2;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("=\"")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(y))(Fay$$list("\""))));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Content) {
            var x = Fay$$_($p1).slot1;
            return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("\"")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(x))(Fay$$list("\"")));
          }
          if (Fay$$_($p1) instanceof $_Cinder$DSL$Complete) {
            return Fay$$list("\u003c--");
          }
          throw ["unhandled case in go",[$p1]];
        });
      };
      return Fay$$_(Fay$$_(Prelude$$36$)(Prelude$concat))(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(cat))(ins))(rm));
    })();
  });
};
var Cinder$DSL$nestLevel = function($p1){
  return function($p2){
    return new Fay$$$(function(){
      var x = $p2;
      if (Fay$$_($p1) instanceof $_Cinder$DSL$Element) {
        return Fay$$_(Fay$$_(Fay$$add)(x))(1);
      }
      var x = $p2;
      if (Fay$$_($p1) instanceof $_Cinder$DSL$Complete) {
        return Fay$$_(Fay$$_(Fay$$sub)(x))(1);
      }
      var x = $p2;
      return x;
    });
  };
};
var Cinder$HTML$Attributes$abbr = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("abbr"));
});
var Cinder$HTML$Attributes$accept = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("accept"));
});
var Cinder$HTML$Attributes$acceptCharset = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("accept-charset"));
});
var Cinder$HTML$Attributes$accesskey = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("accesskey"));
});
var Cinder$HTML$Attributes$action = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("action"));
});
var Cinder$HTML$Attributes$alt = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("alt"));
});
var Cinder$HTML$Attributes$async = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("async"));
});
var Cinder$HTML$Attributes$autocomplete = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("autocomplete"));
});
var Cinder$HTML$Attributes$autofocus = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("autofocus"));
});
var Cinder$HTML$Attributes$autoplay = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("autoplay"));
});
var Cinder$HTML$Attributes$border = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("border"));
});
var Cinder$HTML$Attributes$challenge = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("challenge"));
});
var Cinder$HTML$Attributes$charset = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("charset"));
});
var Cinder$HTML$Attributes$checked = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("checked"));
});
var Cinder$HTML$Attributes$cite = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("cite"));
});
var Cinder$HTML$Attributes$cols = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("cols"));
});
var Cinder$HTML$Attributes$colspan = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("colspan"));
});
var Cinder$HTML$Attributes$command = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("command"));
});
var Cinder$HTML$Attributes$content = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("content"));
});
var Cinder$HTML$Attributes$contenteditable = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("contenteditable"));
});
var Cinder$HTML$Attributes$contextmenu = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("contextmenu"));
});
var Cinder$HTML$Attributes$controls = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("controls"));
});
var Cinder$HTML$Attributes$coords = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("coords"));
});
var Cinder$HTML$Attributes$crossorigin = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("crossorigin"));
});
var Cinder$HTML$Attributes$datetime = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("datetime"));
});
var Cinder$HTML$Attributes$defer = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("defer"));
});
var Cinder$HTML$Attributes$dir = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("dir"));
});
var Cinder$HTML$Attributes$dirname = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("dirname"));
});
var Cinder$HTML$Attributes$disabled = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("disabled"));
});
var Cinder$HTML$Attributes$draggable = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("draggable"));
});
var Cinder$HTML$Attributes$dropzone = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("dropzone"));
});
var Cinder$HTML$Attributes$enctype = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("enctype"));
});
var Cinder$HTML$Attributes$$_for = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("for"));
});
var Cinder$HTML$Attributes$form = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("form"));
});
var Cinder$HTML$Attributes$formaction = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formaction"));
});
var Cinder$HTML$Attributes$formenctype = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formenctype"));
});
var Cinder$HTML$Attributes$formmethod = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formmethod"));
});
var Cinder$HTML$Attributes$formnovalidate = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formnovalidate"));
});
var Cinder$HTML$Attributes$formtarget = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("formtarget"));
});
var Cinder$HTML$Attributes$headers = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("headers"));
});
var Cinder$HTML$Attributes$height = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("height"));
});
var Cinder$HTML$Attributes$hidden = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("hidden"));
});
var Cinder$HTML$Attributes$high = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("high"));
});
var Cinder$HTML$Attributes$href = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("href"));
});
var Cinder$HTML$Attributes$hreflang = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("hreflang"));
});
var Cinder$HTML$Attributes$httpEquiv = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("http-equiv"));
});
var Cinder$HTML$Attributes$icon = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("icon"));
});
var Cinder$HTML$Attributes$id = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("id"));
});
var Cinder$HTML$Attributes$ismap = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("ismap"));
});
var Cinder$HTML$Attributes$keytype = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("keytype"));
});
var Cinder$HTML$Attributes$kind = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("kind"));
});
var Cinder$HTML$Attributes$label = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("label"));
});
var Cinder$HTML$Attributes$lang = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("lang"));
});
var Cinder$HTML$Attributes$list = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("list"));
});
var Cinder$HTML$Attributes$loop = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("loop"));
});
var Cinder$HTML$Attributes$low = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("low"));
});
var Cinder$HTML$Attributes$manifest = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("manifest"));
});
var Cinder$HTML$Attributes$max = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("max"));
});
var Cinder$HTML$Attributes$maxlength = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("maxlength"));
});
var Cinder$HTML$Attributes$media = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("media"));
});
var Cinder$HTML$Attributes$mediagroup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("mediagroup"));
});
var Cinder$HTML$Attributes$method = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("method"));
});
var Cinder$HTML$Attributes$min = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("min"));
});
var Cinder$HTML$Attributes$multiple = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("multiple"));
});
var Cinder$HTML$Attributes$muted = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("muted"));
});
var Cinder$HTML$Attributes$name = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("name"));
});
var Cinder$HTML$Attributes$novalidate = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("novalidate"));
});
var Cinder$HTML$Attributes$open = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("open"));
});
var Cinder$HTML$Attributes$optimum = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("optimum"));
});
var Cinder$HTML$Attributes$pattern = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("pattern"));
});
var Cinder$HTML$Attributes$placeholder = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("placeholder"));
});
var Cinder$HTML$Attributes$poster = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("poster"));
});
var Cinder$HTML$Attributes$preload = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("preload"));
});
var Cinder$HTML$Attributes$radiogroup = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("radiogroup"));
});
var Cinder$HTML$Attributes$readonly = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("readonly"));
});
var Cinder$HTML$Attributes$rel = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("rel"));
});
var Cinder$HTML$Attributes$required = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("required"));
});
var Cinder$HTML$Attributes$reversed = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("reversed"));
});
var Cinder$HTML$Attributes$rows = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("rows"));
});
var Cinder$HTML$Attributes$rowspan = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("rowspan"));
});
var Cinder$HTML$Attributes$sandbox = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("sandbox"));
});
var Cinder$HTML$Attributes$scope = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("scope"));
});
var Cinder$HTML$Attributes$scoped = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("scoped"));
});
var Cinder$HTML$Attributes$seamless = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("seamless"));
});
var Cinder$HTML$Attributes$selected = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("selected"));
});
var Cinder$HTML$Attributes$shape = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("shape"));
});
var Cinder$HTML$Attributes$size = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("size"));
});
var Cinder$HTML$Attributes$sizes = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("sizes"));
});
var Cinder$HTML$Attributes$span = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("span"));
});
var Cinder$HTML$Attributes$spellcheck = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("spellcheck"));
});
var Cinder$HTML$Attributes$src = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("src"));
});
var Cinder$HTML$Attributes$srcdoc = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("srcdoc"));
});
var Cinder$HTML$Attributes$srclang = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("srclang"));
});
var Cinder$HTML$Attributes$start = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("start"));
});
var Cinder$HTML$Attributes$step = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("step"));
});
var Cinder$HTML$Attributes$style = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("style"));
});
var Cinder$HTML$Attributes$tabindex = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("tabindex"));
});
var Cinder$HTML$Attributes$target = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("target"));
});
var Cinder$HTML$Attributes$title = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("title"));
});
var Cinder$HTML$Attributes$translate = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("translate"));
});
var Cinder$HTML$Attributes$typemustmatch = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("typemustmatch"));
});
var Cinder$HTML$Attributes$usemap = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("usemap"));
});
var Cinder$HTML$Attributes$value = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("value"));
});
var Cinder$HTML$Attributes$width = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("width"));
});
var Cinder$HTML$Attributes$wrap = new Fay$$$(function(){
  return Fay$$_(Cinder$DSL$Attribute)(Fay$$list("wrap"));
});
var Table$colors = new Fay$$$(function(){
  var s = new Fay$$$(function(){
    return Prelude$enumFromThenTo(0)(51)(255);
  });
  var go = function($p1){
    return new Fay$$$(function(){
      if (Fay$$_($p1) === null) {
        return null;
      }
      var xs = $p1;
      return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Prelude$map)(Cinder$DOM$rgb))(Fay$$_(Fay$$_(Prelude$take)(24))(xs))))(Fay$$_(go)(Fay$$_(Fay$$_(Prelude$drop)(24))(xs)));
    });
  };
  return Fay$$_(go)((function(){
    var $gen_1 = function($p1){
      return new Fay$$$(function(){
        var r = $p1;
        return (function(){
          var $gen_1 = function($p1){
            return new Fay$$$(function(){
              var g = $p1;
              return (function(){
                var $gen_1 = function($p1){
                  return new Fay$$$(function(){
                    var b = $p1;
                    return Fay$$list([Fay$$list([r,g,b])]);
                    return null;
                  });
                };
                return Fay$$_(Fay$$_(Prelude$concatMap)($gen_1))(s);
              })();
              return null;
            });
          };
          return Fay$$_(Fay$$_(Prelude$concatMap)($gen_1))(s);
        })();
        return null;
      });
    };
    return Fay$$_(Fay$$_(Prelude$concatMap)($gen_1))(s);
  })());
});
var Table$tableMU = new Fay$$$(function(){
  var row = function($p1){
    return new Fay$$$(function(){
      var x = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Fay$$_(Cinder$DSL$$33$$43$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(Cinder$DSL$markup))(Cinder$HTML$Elements$tr)))(Fay$$_(Fay$$_(Prelude$foldl1)(Cinder$DSL$$33$$43$))(Fay$$_(Fay$$_(Prelude$map)(box))(x)))))(Cinder$DSL$Complete);
    });
  };
  var box = function($p1){
    return new Fay$$$(function(){
      var x = $p1;
      return Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(Cinder$DSL$markup))(Cinder$HTML$Elements$td)))(Fay$$_(sty)(x))))(Fay$$_(Cinder$HTML$inner)(Fay$$list("&#955;")))))(Cinder$DSL$Complete);
    });
  };
  var sty = function($p1){
    return new Fay$$$(function(){
      var x = $p1;
      return Fay$$_(Cinder$HTML$Attributes$style)(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("height:20px;width:20px;background-color:")))(x));
    });
  };
  return Fay$$_(Fay$$_(Cinder$DSL$$33$)(Fay$$_(Fay$$_(Cinder$DSL$$33$$43$)(Fay$$_(Fay$$_(Cinder$DSL$$33$)(Cinder$DSL$markup))(Cinder$HTML$Elements$table)))(Fay$$_(Fay$$_(Prelude$foldl1)(Cinder$DSL$$33$$43$))(Fay$$_(Fay$$_(Prelude$map)(row))(Table$colors)))))(Cinder$DSL$Complete);
});
var Table$htex = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$bind)(Cinder$DOM$root))(Fay$$_(Cinder$DOM$byTag)(Fay$$list("body")))))(Fay$$_(Fay$$_(Control$Fay$zipWithM_)(Cinder$HTML$insert))(Fay$$list([Table$tableMU])));
});
var Table$main = new Fay$$$(function(){
  return Fay$$_(Fay$$_(Fay$$_(Table$addEventListener)(Fay$$list("load")))(Table$htex))(false);
});
var Table$addEventListener = function($p1){
  return function($p2){
    return function($p3){
      return new Fay$$$(function(){
        return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['addEventListener'](Fay$$fayToJs_string($p1),Fay$$fayToJs(["action",[["unknown"]]],$p2),Fay$$fayToJs_bool($p3))));
      });
    };
  };
};
var $_Language$Fay$FFI$Nullable = function(slot1){
  this.slot1 = slot1;
};
var $_Language$Fay$FFI$Null = function(){
};
var $_Language$Fay$FFI$Defined = function(slot1){
  this.slot1 = slot1;
};
var $_Language$Fay$FFI$Undefined = function(){
};
var $_Prelude$Just = function(slot1){
  this.slot1 = slot1;
};
var $_Prelude$Nothing = function(){
};
var $_Prelude$Left = function(slot1){
  this.slot1 = slot1;
};
var $_Prelude$Right = function(slot1){
  this.slot1 = slot1;
};
var $_Prelude$Ratio = function(slot1,slot2){
  this.slot1 = slot1;
  this.slot2 = slot2;
};
var $_Prelude$GT = function(){
};
var $_Prelude$LT = function(){
};
var $_Prelude$EQ = function(){
};
var $_Language$Fay$FFI$Nullable = function(slot1){
  this.slot1 = slot1;
};
var $_Language$Fay$FFI$Null = function(){
};
var $_Language$Fay$FFI$Defined = function(slot1){
  this.slot1 = slot1;
};
var $_Language$Fay$FFI$Undefined = function(){
};
var $_Cinder$DSL$Content = function(slot1){
  this.slot1 = slot1;
};
var $_Cinder$DSL$Element = function(slot1){
  this.slot1 = slot1;
};
var $_Cinder$DSL$Attribute = function(slot1,slot2){
  this.slot1 = slot1;
  this.slot2 = slot2;
};
var $_Cinder$DSL$Property = function(slot1,slot2){
  this.slot1 = slot1;
  this.slot2 = slot2;
};
var $_Cinder$DSL$Complete = function(){
};
var $_Cinder$DSL$Content = function(slot1){
  this.slot1 = slot1;
};
var $_Cinder$DSL$Element = function(slot1){
  this.slot1 = slot1;
};
var $_Cinder$DSL$Attribute = function(slot1,slot2){
  this.slot1 = slot1;
  this.slot2 = slot2;
};
var $_Cinder$DSL$Property = function(slot1,slot2){
  this.slot1 = slot1;
  this.slot2 = slot2;
};
var $_Cinder$DSL$Complete = function(){
};
var $_Cinder$DSL$Content = function(slot1){
  this.slot1 = slot1;
};
var $_Cinder$DSL$Element = function(slot1){
  this.slot1 = slot1;
};
var $_Cinder$DSL$Attribute = function(slot1,slot2){
  this.slot1 = slot1;
  this.slot2 = slot2;
};
var $_Cinder$DSL$Property = function(slot1,slot2){
  this.slot1 = slot1;
  this.slot2 = slot2;
};
var $_Cinder$DSL$Complete = function(){
};
var Fay$$fayToJsUserDefined = function(type,obj){
  var _obj = Fay$$_(obj);
  var argTypes = type[2];
  if (_obj instanceof $_Language$Fay$FFI$Nullable) {
    var obj_ = {"instance": "Nullable"};
    var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$FFI$Null) {
    var obj_ = {"instance": "Null"};
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$FFI$Defined) {
    var obj_ = {"instance": "Defined"};
    var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$FFI$Undefined) {
    var obj_ = {"instance": "Undefined"};
    return obj_;
  }
  if (_obj instanceof $_Prelude$Just) {
    var obj_ = {"instance": "Just"};
    var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Prelude$Nothing) {
    var obj_ = {"instance": "Nothing"};
    return obj_;
  }
  if (_obj instanceof $_Prelude$Left) {
    var obj_ = {"instance": "Left"};
    var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Prelude$Right) {
    var obj_ = {"instance": "Right"};
    var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Prelude$Ratio) {
    var obj_ = {"instance": "Ratio"};
    var obj_slot1 = Fay$$fayToJs_int(_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    var obj_slot2 = Fay$$fayToJs_int(_obj.slot2);
    if (undefined !== obj_slot2) {
      obj_['slot2'] = obj_slot2;
    }
    return obj_;
  }
  if (_obj instanceof $_Prelude$GT) {
    var obj_ = {"instance": "GT"};
    return obj_;
  }
  if (_obj instanceof $_Prelude$LT) {
    var obj_ = {"instance": "LT"};
    return obj_;
  }
  if (_obj instanceof $_Prelude$EQ) {
    var obj_ = {"instance": "EQ"};
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$FFI$Nullable) {
    var obj_ = {"instance": "Nullable"};
    var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$FFI$Null) {
    var obj_ = {"instance": "Null"};
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$FFI$Defined) {
    var obj_ = {"instance": "Defined"};
    var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Language$Fay$FFI$Undefined) {
    var obj_ = {"instance": "Undefined"};
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Content) {
    var obj_ = {"instance": "Content"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Element) {
    var obj_ = {"instance": "Element"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Attribute) {
    var obj_ = {"instance": "Attribute"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    var obj_slot2 = Fay$$fayToJs(["user","DString",[]],_obj.slot2);
    if (undefined !== obj_slot2) {
      obj_['slot2'] = obj_slot2;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Property) {
    var obj_ = {"instance": "Property"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    var obj_slot2 = Fay$$fayToJs(["user","DString",[]],_obj.slot2);
    if (undefined !== obj_slot2) {
      obj_['slot2'] = obj_slot2;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Complete) {
    var obj_ = {"instance": "Complete"};
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Content) {
    var obj_ = {"instance": "Content"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Element) {
    var obj_ = {"instance": "Element"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Attribute) {
    var obj_ = {"instance": "Attribute"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    var obj_slot2 = Fay$$fayToJs(["user","DString",[]],_obj.slot2);
    if (undefined !== obj_slot2) {
      obj_['slot2'] = obj_slot2;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Property) {
    var obj_ = {"instance": "Property"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    var obj_slot2 = Fay$$fayToJs(["user","DString",[]],_obj.slot2);
    if (undefined !== obj_slot2) {
      obj_['slot2'] = obj_slot2;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Complete) {
    var obj_ = {"instance": "Complete"};
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Content) {
    var obj_ = {"instance": "Content"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Element) {
    var obj_ = {"instance": "Element"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Attribute) {
    var obj_ = {"instance": "Attribute"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    var obj_slot2 = Fay$$fayToJs(["user","DString",[]],_obj.slot2);
    if (undefined !== obj_slot2) {
      obj_['slot2'] = obj_slot2;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Property) {
    var obj_ = {"instance": "Property"};
    var obj_slot1 = Fay$$fayToJs(["user","DString",[]],_obj.slot1);
    if (undefined !== obj_slot1) {
      obj_['slot1'] = obj_slot1;
    }
    var obj_slot2 = Fay$$fayToJs(["user","DString",[]],_obj.slot2);
    if (undefined !== obj_slot2) {
      obj_['slot2'] = obj_slot2;
    }
    return obj_;
  }
  if (_obj instanceof $_Cinder$DSL$Complete) {
    var obj_ = {"instance": "Complete"};
    return obj_;
  }
  return obj;
};
var Fay$$jsToFayUserDefined = function(type,obj){
  var argTypes = type[2];
  if (obj["instance"] === "Nullable") {
    return new $_Language$Fay$FFI$Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "Null") {
    return new $_Language$Fay$FFI$Null();
  }
  if (obj["instance"] === "Defined") {
    return new $_Language$Fay$FFI$Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "Undefined") {
    return new $_Language$Fay$FFI$Undefined();
  }
  if (obj["instance"] === "Just") {
    return new $_Prelude$Just(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "Nothing") {
    return new $_Prelude$Nothing();
  }
  if (obj["instance"] === "Left") {
    return new $_Prelude$Left(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "Right") {
    return new $_Prelude$Right(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "Ratio") {
    return new $_Prelude$Ratio(Fay$$jsToFay_int(obj["slot1"]),Fay$$jsToFay_int(obj["slot2"]));
  }
  if (obj["instance"] === "GT") {
    return new $_Prelude$GT();
  }
  if (obj["instance"] === "LT") {
    return new $_Prelude$LT();
  }
  if (obj["instance"] === "EQ") {
    return new $_Prelude$EQ();
  }
  if (obj["instance"] === "Nullable") {
    return new $_Language$Fay$FFI$Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "Null") {
    return new $_Language$Fay$FFI$Null();
  }
  if (obj["instance"] === "Defined") {
    return new $_Language$Fay$FFI$Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));
  }
  if (obj["instance"] === "Undefined") {
    return new $_Language$Fay$FFI$Undefined();
  }
  if (obj["instance"] === "Content") {
    return new $_Cinder$DSL$Content(Fay$$jsToFay(["user","DString",[]],obj["slot1"]));
  }
  if (obj["instance"] === "Element") {
    return new $_Cinder$DSL$Element(Fay$$jsToFay(["user","DString",[]],obj["slot1"]));
  }
  if (obj["instance"] === "Attribute") {
    return new $_Cinder$DSL$Attribute(Fay$$jsToFay(["user","DString",[]],obj["slot1"]),Fay$$jsToFay(["user","DString",[]],obj["slot2"]));
  }
  if (obj["instance"] === "Property") {
    return new $_Cinder$DSL$Property(Fay$$jsToFay(["user","DString",[]],obj["slot1"]),Fay$$jsToFay(["user","DString",[]],obj["slot2"]));
  }
  if (obj["instance"] === "Complete") {
    return new $_Cinder$DSL$Complete();
  }
  if (obj["instance"] === "Content") {
    return new $_Cinder$DSL$Content(Fay$$jsToFay(["user","DString",[]],obj["slot1"]));
  }
  if (obj["instance"] === "Element") {
    return new $_Cinder$DSL$Element(Fay$$jsToFay(["user","DString",[]],obj["slot1"]));
  }
  if (obj["instance"] === "Attribute") {
    return new $_Cinder$DSL$Attribute(Fay$$jsToFay(["user","DString",[]],obj["slot1"]),Fay$$jsToFay(["user","DString",[]],obj["slot2"]));
  }
  if (obj["instance"] === "Property") {
    return new $_Cinder$DSL$Property(Fay$$jsToFay(["user","DString",[]],obj["slot1"]),Fay$$jsToFay(["user","DString",[]],obj["slot2"]));
  }
  if (obj["instance"] === "Complete") {
    return new $_Cinder$DSL$Complete();
  }
  if (obj["instance"] === "Content") {
    return new $_Cinder$DSL$Content(Fay$$jsToFay(["user","DString",[]],obj["slot1"]));
  }
  if (obj["instance"] === "Element") {
    return new $_Cinder$DSL$Element(Fay$$jsToFay(["user","DString",[]],obj["slot1"]));
  }
  if (obj["instance"] === "Attribute") {
    return new $_Cinder$DSL$Attribute(Fay$$jsToFay(["user","DString",[]],obj["slot1"]),Fay$$jsToFay(["user","DString",[]],obj["slot2"]));
  }
  if (obj["instance"] === "Property") {
    return new $_Cinder$DSL$Property(Fay$$jsToFay(["user","DString",[]],obj["slot1"]),Fay$$jsToFay(["user","DString",[]],obj["slot2"]));
  }
  if (obj["instance"] === "Complete") {
    return new $_Cinder$DSL$Complete();
  }
  return obj;
};

// Exports
this.Table$main = Table$main;

// Built-ins
this._ = Fay$$_;
this.$           = Fay$$$;
this.$fayToJs    = Fay$$fayToJs;
this.$jsToFay    = Fay$$jsToFay;

};
;
var main = new Table();
main._(main.Table$main);

