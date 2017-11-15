// Generated by psc-bundle 0.11.6
var PS = {};
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var Control_Category = PS["Control.Category"];
  var $$const = function (a) {
      return function (v) {
          return a;
      };
  };
  exports["const"] = $$const;
})(PS["Data.Function"] = PS["Data.Function"] || {});
(function(exports) {
    "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Data.Unit"];
  var Data_Show = PS["Data.Show"];
  exports["unit"] = $foreign.unit;
})(PS["Data.Unit"] = PS["Data.Unit"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Data.Functor"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_Function = PS["Data.Function"];
  var Data_Unit = PS["Data.Unit"];        
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  var $$void = function (dictFunctor) {
      return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["void"] = $$void;
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];        
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS["Control.Apply"] = PS["Control.Apply"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var Control_Apply = PS["Control.Apply"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["liftA1"] = liftA1;
  exports["pure"] = pure;
})(PS["Control.Applicative"] = PS["Control.Applicative"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Control.Bind"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Category = PS["Control.Category"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  exports["Bind"] = Bind;
  exports["bind"] = bind;
})(PS["Control.Bind"] = PS["Control.Bind"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS["Control.Monad"] = PS["Control.Monad"] || {});
(function(exports) {
    "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Control.Monad.Eff"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad = PS["Control.Monad"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Unit = PS["Data.Unit"];        
  var monadEff = new Control_Monad.Monad(function () {
      return applicativeEff;
  }, function () {
      return bindEff;
  });
  var bindEff = new Control_Bind.Bind(function () {
      return applyEff;
  }, $foreign.bindE);
  var applyEff = new Control_Apply.Apply(function () {
      return functorEff;
  }, Control_Monad.ap(monadEff));
  var applicativeEff = new Control_Applicative.Applicative(function () {
      return applyEff;
  }, $foreign.pureE);
  var functorEff = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEff));
  exports["functorEff"] = functorEff;
  exports["applyEff"] = applyEff;
  exports["applicativeEff"] = applicativeEff;
  exports["bindEff"] = bindEff;
  exports["monadEff"] = monadEff;
})(PS["Control.Monad.Eff"] = PS["Control.Monad.Eff"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var Control_Alt = PS["Control.Alt"];
  var Control_Alternative = PS["Control.Alternative"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Apply = PS["Control.Apply"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Category = PS["Control.Category"];
  var Control_Extend = PS["Control.Extend"];
  var Control_Monad = PS["Control.Monad"];
  var Control_MonadZero = PS["Control.MonadZero"];
  var Control_Plus = PS["Control.Plus"];
  var Data_Bounded = PS["Data.Bounded"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Functor_Invariant = PS["Data.Functor.Invariant"];
  var Data_Monoid = PS["Data.Monoid"];
  var Data_Ord = PS["Data.Ord"];
  var Data_Ordering = PS["Data.Ordering"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Data_Unit = PS["Data.Unit"];
  var Prelude = PS["Prelude"];        
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
})(PS["Data.Maybe"] = PS["Data.Maybe"] || {});
(function(exports) {
  /* global exports */
  "use strict";

  exports.getCanvasElementByIdImpl = function(id, Just, Nothing) {
      return function() {
          var el = document.getElementById(id);
          if (el && el instanceof HTMLCanvasElement) {
              return Just(el);
          } else {
              return Nothing;
          }
      };
  };

  exports.getContext2D = function(c) {
      return function() {
          return c.getContext('2d');
      };
  };

  exports.setStrokeStyle = function(style) {
      return function(ctx) {
          return function() {
              ctx.strokeStyle = style;
              return ctx;
          };
      };
  };

  exports.beginPath = function(ctx) {
      return function() {
          ctx.beginPath();
          return ctx;
      };
  };

  exports.stroke = function(ctx) {
      return function() {
          ctx.stroke();
          return ctx;
      };
  };

  exports.lineTo = function(ctx) {
      return function(x) {
          return function(y) {
              return function() {
                  ctx.lineTo(x, y);
                  return ctx;
              };
          };
      };
  };

  exports.moveTo = function(ctx) {
      return function(x) {
          return function(y) {
              return function() {
                  ctx.moveTo(x, y);
                  return ctx;
              };
          };
      };
  };

  exports.closePath = function(ctx) {
      return function() {
          ctx.closePath();
          return ctx;
      };
  };

  exports.save = function(ctx) {
      return function() {
          ctx.save();
          return ctx;
      };
  };

  exports.restore = function(ctx) {
      return function() {
          ctx.restore();
          return ctx;
      };
  };
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Graphics.Canvas"];
  var Control_Applicative = PS["Control.Applicative"];
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Exception_Unsafe = PS["Control.Monad.Eff.Exception.Unsafe"];
  var Control_Semigroupoid = PS["Control.Semigroupoid"];
  var Data_ArrayBuffer_Types = PS["Data.ArrayBuffer.Types"];
  var Data_Eq = PS["Data.Eq"];
  var Data_Function = PS["Data.Function"];
  var Data_Function_Uncurried = PS["Data.Function.Uncurried"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Semigroup = PS["Data.Semigroup"];
  var Data_Show = PS["Data.Show"];
  var Prelude = PS["Prelude"];
  var withContext = function (ctx) {
      return function (action) {
          return function __do() {
              var v = $foreign.save(ctx)();
              var v1 = action();
              var v2 = $foreign.restore(ctx)();
              return v1;
          };
      };
  };
  var getCanvasElementById = function (elId) {
      return $foreign.getCanvasElementByIdImpl(elId, Data_Maybe.Just.create, Data_Maybe.Nothing.value);
  };
  exports["getCanvasElementById"] = getCanvasElementById;
  exports["withContext"] = withContext;
  exports["beginPath"] = $foreign.beginPath;
  exports["closePath"] = $foreign.closePath;
  exports["getContext2D"] = $foreign.getContext2D;
  exports["lineTo"] = $foreign.lineTo;
  exports["moveTo"] = $foreign.moveTo;
  exports["setStrokeStyle"] = $foreign.setStrokeStyle;
  exports["stroke"] = $foreign.stroke;
})(PS["Graphics.Canvas"] = PS["Graphics.Canvas"] || {});
(function(exports) {
    "use strict";          

  exports.cos = Math.cos;    

  exports.sin = Math.sin;      

  exports.pi = Math.PI;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var $foreign = PS["Math"];
  exports["cos"] = $foreign.cos;
  exports["pi"] = $foreign.pi;
  exports["sin"] = $foreign.sin;
})(PS["Math"] = PS["Math"] || {});
(function(exports) {
  // Generated by purs version 0.11.6
  "use strict";
  var Control_Bind = PS["Control.Bind"];
  var Control_Monad_Eff = PS["Control.Monad.Eff"];
  var Control_Monad_Eff_Ref = PS["Control.Monad.Eff.Ref"];
  var DOM = PS["DOM"];
  var DOM_HTML = PS["DOM.HTML"];
  var DOM_HTML_Types = PS["DOM.HTML.Types"];
  var DOM_HTML_Window = PS["DOM.HTML.Window"];
  var Data_EuclideanRing = PS["Data.EuclideanRing"];
  var Data_Function = PS["Data.Function"];
  var Data_Functor = PS["Data.Functor"];
  var Data_Maybe = PS["Data.Maybe"];
  var Data_Ring = PS["Data.Ring"];
  var Data_Semiring = PS["Data.Semiring"];
  var Graphics_Canvas = PS["Graphics.Canvas"];
  var $$Math = PS["Math"];
  var Partial_Unsafe = PS["Partial.Unsafe"];
  var Prelude = PS["Prelude"];        
  var Point3D = function (x) {
      return x;
  };
  var Point2D = function (x) {
      return x;
  };
  var Cube = function (x) {
      return x;
  };
  var Angle3D = function (x) {
      return x;
  };
  var withStroke = function (ctx) {
      return function (color) {
          return function (draw) {
              return Graphics_Canvas.withContext(ctx)(function __do() {
                  var v = Graphics_Canvas.setStrokeStyle(color)(ctx)();
                  var v1 = Graphics_Canvas.beginPath(v)();
                  var v2 = draw(v1)();
                  var v3 = Graphics_Canvas.closePath(v2)();
                  return Graphics_Canvas.stroke(v3)();
              });
          };
      };
  };
  var project = function (v) {
      return function (v1) {
          var yRotQz = v.y * $$Math.cos(v1.qz) - v.x * $$Math.sin(v1.qz);
          var yRotQzQx = yRotQz * $$Math.cos(v1.qx) + v.z * $$Math.sin(v1.qx);
          var zRotQzQx = v.z * $$Math.cos(v1.qx) - yRotQz * $$Math.sin(v1.qx);
          var xRotQz = v.x * $$Math.cos(v1.qz) + v.y * $$Math.sin(v1.qz);
          var xRotQzQxQy = xRotQz * $$Math.cos(v1.qy) + zRotQzQx * $$Math.sin(v1.qy);
          return {
              x: xRotQzQxQy, 
              y: yRotQzQx
          };
      };
  };
  var drawLine = function (ctx) {
      return function (v) {
          return function (v1) {
              return function __do() {
                  var v2 = Graphics_Canvas.moveTo(ctx)(v.x)(v.y)();
                  return Graphics_Canvas.lineTo(v2)(v1.x)(v1.y)();
              };
          };
      };
  };
  var drawCube = function (ctx) {
      return function (v) {
          return function (v1) {
              var half = v.size / 2.0;
              var v11 = project({
                  x: v.x - half, 
                  y: v.y - half, 
                  z: v.z - half
              })({
                  qx: v1.qx, 
                  qy: v1.qy, 
                  qz: v1.qz
              });
              var v2 = project({
                  x: v.x - half, 
                  y: v.y + half, 
                  z: v.z - half
              })({
                  qx: v1.qx, 
                  qy: v1.qy, 
                  qz: v1.qz
              });
              var v3 = project({
                  x: v.x - half, 
                  y: v.y - half, 
                  z: v.z + half
              })({
                  qx: v1.qx, 
                  qy: v1.qy, 
                  qz: v1.qz
              });
              var v4 = project({
                  x: v.x - half, 
                  y: v.y + half, 
                  z: v.z + half
              })({
                  qx: v1.qx, 
                  qy: v1.qy, 
                  qz: v1.qz
              });
              var v5 = project({
                  x: v.x + half, 
                  y: v.y - half, 
                  z: v.z - half
              })({
                  qx: v1.qx, 
                  qy: v1.qy, 
                  qz: v1.qz
              });
              var v6 = project({
                  x: v.x + half, 
                  y: v.y + half, 
                  z: v.z - half
              })({
                  qx: v1.qx, 
                  qy: v1.qy, 
                  qz: v1.qz
              });
              var v7 = project({
                  x: v.x + half, 
                  y: v.y - half, 
                  z: v.z + half
              })({
                  qx: v1.qx, 
                  qy: v1.qy, 
                  qz: v1.qz
              });
              var v8 = project({
                  x: v.x + half, 
                  y: v.y + half, 
                  z: v.z + half
              })({
                  qx: v1.qx, 
                  qy: v1.qy, 
                  qz: v1.qz
              });
              return withStroke(ctx)(v.color)(function (ctx2) {
                  return function __do() {
                      var v9 = drawLine(ctx2)(v11)(v5)();
                      var v10 = drawLine(v9)(v5)(v6)();
                      var v12 = drawLine(v10)(v6)(v2)();
                      var v13 = drawLine(v12)(v2)(v11)();
                      var v14 = drawLine(v13)(v3)(v7)();
                      var v15 = drawLine(v14)(v7)(v8)();
                      var v16 = drawLine(v15)(v8)(v4)();
                      var v17 = drawLine(v16)(v4)(v3)();
                      var v18 = drawLine(v17)(v11)(v3)();
                      var v19 = drawLine(v18)(v5)(v7)();
                      var v20 = drawLine(v19)(v6)(v8)();
                      return drawLine(v20)(v2)(v4)();
                  };
              });
          };
      };
  };
  var main = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
      var v = Graphics_Canvas.getCanvasElementById("thecanvas")();
      var __unused = function (dictPartial1) {
          return function ($dollar25) {
              return $dollar25;
          };
      };
      return __unused()((function () {
          if (v instanceof Data_Maybe.Just) {
              return function __do() {
                  var v1 = Graphics_Canvas.getContext2D(v.value0)();
                  return drawCube(v1)({
                      x: 300.0, 
                      y: 600.0, 
                      z: 0.0, 
                      size: 200.0, 
                      color: "rgb(0,0,0)"
                  })({
                      qx: $$Math.pi / 4.0, 
                      qy: $$Math.pi / 3.0, 
                      qz: $$Math.pi / 4.0
                  })();
              };
          };
          throw new Error("Failed pattern match at Main line 110, column 3 - line 111, column 3: " + [ v.constructor.name ]);
      })())();
  });
  exports["Angle3D"] = Angle3D;
  exports["Cube"] = Cube;
  exports["Point2D"] = Point2D;
  exports["Point3D"] = Point3D;
  exports["drawCube"] = drawCube;
  exports["drawLine"] = drawLine;
  exports["main"] = main;
  exports["project"] = project;
  exports["withStroke"] = withStroke;
})(PS["Main"] = PS["Main"] || {});
PS["Main"].main();
