using System;

using UnityEngine;

namespace Prolog
{
    public static class FunctionalExpression
    {
        public static object Eval(object term, PrologContext context)
        {
            term = Term.Deref(term);
            var indexical = term as Indexical;
            if (indexical != null)
                return indexical.GetValue(context);
            var offendingVariable = term as LogicVariable;
            if (offendingVariable != null)
                throw new InstantiationException(offendingVariable,
                    "arithmetic expression cannot be evaluated because it includes an uninstantiated variable.");
            var t = term as Structure;
            if (t == null)
            {
                //var s = term as Symbol;
                //if (s != null)
                //    throw new BadProcedureException(s, 0);
                //throw new BadProcedureException(term);
                return term;  // It's a literal.
            }
            switch (t.Functor.Name)
            {
                case "+":
                    if (t.Arguments.Length != 2)
                        throw new ArgumentCountException("+", t.Arguments, "number1", "number2");
                    return GenericArithmetic.Add(Eval(t.Arguments[0], context), Eval(t.Arguments[1], context));

                case "-":
                    if (t.Arguments.Length == 2)
                        return GenericArithmetic.Subtract(Eval(t.Arguments[0], context),
                            Eval(t.Arguments[1], context));
                    if (t.Arguments.Length == 1)
                        return GenericArithmetic.Subtract(Eval(t.Arguments[0], context));
                    throw new ArgumentException("Wrong number of arguments in - expression; should be 1 or 2.");

                case "*":
                    if (t.Arguments.Length != 2)
                        throw new ArgumentCountException("*", t.Arguments, "number1", "number2");
                    return GenericArithmetic.Multiply(Eval(t.Arguments[0], context),
                        Eval(t.Arguments[1], context));

                case "/":
                    if (t.Arguments.Length != 2)
                        throw new ArgumentCountException("/", t.Arguments, "number1", "number2");
                    return GenericArithmetic.Divide(Eval(t.Arguments[0], context),
                        Eval(t.Arguments[1], context));

                case "mod":
                    if (t.Arguments.Length != 2)
                        throw new ArgumentCountException("mod", t.Arguments, "number1", "number2");
                    return Convert.ToInt32(Eval(t.Arguments[0], context))%
                           Convert.ToInt32((Eval(t.Arguments[1], context)));

                case "//":
                    if (t.Arguments.Length != 2)
                        throw new ArgumentCountException("//", t.Arguments, "number1", "number2");
                    return Convert.ToInt32(Eval(t.Arguments[0], context))/
                           Convert.ToInt32(Eval(t.Arguments[1], context));

                case "sqrt":
                    if (t.Arguments.Length != 1) throw new ArgumentCountException("sqrt", t.Arguments, "number");
                    return Math.Sqrt(Convert.ToDouble(Eval(t.Arguments[0], context)));

                case "abs":
                    if (t.Arguments.Length != 1) throw new ArgumentCountException("abs", t.Arguments, "number");
                    return Math.Abs(Convert.ToDouble(Eval(t.Arguments[0], context)));

                case "log":
                    if (t.Arguments.Length != 1) throw new ArgumentCountException("log", t.Arguments, "number");
                    return Math.Log(Convert.ToDouble(Eval(t.Arguments[0], context)));

                case "exp":
                    if (t.Arguments.Length != 1) throw new ArgumentCountException("exp", t.Arguments, "number");
                    return Math.Exp(Convert.ToDouble(Eval(t.Arguments[0], context)));

                case "floor":
                    if (t.Arguments.Length != 1) throw new ArgumentCountException("floor", t.Arguments, "number");
                    return Math.Floor(Convert.ToDouble(Eval(t.Arguments[0], context)));

                case "float":
                    if (t.Arguments.Length != 1) throw new ArgumentCountException("floor", t.Arguments, "number");
                    return Convert.ToSingle(Eval(t.Arguments[0], context));

		case "min":
                    if (t.Arguments.Length != 2)
                        throw new ArgumentCountException("min", t.Arguments, "number1", "number2");
                    return GenericArithmetic.Min(Eval(t.Arguments[0], context), Eval(t.Arguments[1], context));

		case "max":
                    if (t.Arguments.Length != 2)
                        throw new ArgumentCountException("max", t.Arguments, "number1", "number2");
                    return GenericArithmetic.Max(Eval(t.Arguments[0], context), Eval(t.Arguments[1], context));

                case "magnitude":
                {
                    if (t.Arguments.Length != 1)
                        throw new ArgumentCountException("magnitude", t.Arguments, "Vector3");
                    object v = Eval(t.Argument(0), context);
                    if (!(v is Vector3))
                        throw new ArgumentTypeException("magnitude", "vector", v, typeof (Vector3));
                    return ((Vector3) v).magnitude;
                }

                case "magnitude_squared":
                {
                    if (t.Arguments.Length != 1)
                        throw new ArgumentCountException("magnitude_squared", t.Arguments, "Vector3");
                    object v = Eval(t.Argument(0), context);
                    if (!(v is Vector3))
                        throw new ArgumentTypeException("magnitude_squared", "vector", v, typeof (Vector3));
                    return ((Vector3) v).sqrMagnitude;
                }

                case "distance":
                {
                    if (t.Arguments.Length != 2)
                        throw new ArgumentCountException("distance", t.Arguments, "v1", "v2");
                    object v1 = Eval(t.Argument(0), context);
                    if (v1 is GameObject)
                        v1 = ((GameObject)v1).transform.position;
                    object v2 = Eval(t.Argument(1), context);
                    if (v2 is GameObject)
                        v2 = ((GameObject)v2).transform.position;
                    if (!(v1 is Vector3))
                        throw new ArgumentTypeException("distance", "v1", v1, typeof (Vector3));
                    if (!(v2 is Vector3))
                        throw new ArgumentTypeException("distance", "v2", v2, typeof (Vector3));
                    return Vector3.Distance((Vector3) v1, (Vector3) v2);
                }

                case "distance_squared":
                {
                    if (t.Arguments.Length != 2)
                        throw new ArgumentCountException("distance_squared", t.Arguments, "v1", "v2");
                    object v1 = Eval(t.Argument(0), context);
                    if (v1 is GameObject)
                        v1 = ((GameObject)v1).transform.position;
                    object v2 = Eval(t.Argument(1), context);
                    if (v2 is GameObject)
                        v2 = ((GameObject)v2).transform.position;
                    if (!(v1 is Vector3))
                        throw new ArgumentTypeException("distance_squared", "v1", v1, typeof (Vector3));
                    if (!(v2 is Vector3))
                        throw new ArgumentTypeException("distance_squared", "v2", v2, typeof (Vector3));
                    return Vector3.SqrMagnitude((Vector3) v1 - (Vector3) v2);
                }

                case "position":
                {
                    if (t.Arguments.Length != 1)
                        throw new ArgumentCountException("position", t.Arguments, "gameObject");
                    var gameObject = Eval(t.Argument(0), context);
                    var go = gameObject as GameObject;
                    if (go==null)
                        throw new ArgumentTypeException("position", "gameObject", gameObject, typeof(GameObject));
                    return go.transform.position;
                }

                case ".":
                    if (t.Arguments.Length != 2)
                    {
                        throw new ArgumentCountException(".", t.Arguments, "object");
                    }
                    return EvalMemberExpression(t.Arguments[0], t.Arguments[1], context);

                case "property":
                {
                    if (t.Arguments.Length != 2)
                        throw new ArgumentCountException("property", t.Arguments, "object", "property_name");
                    object o = t.Argument(0);
                    if (o is Structure)
                        o = Eval(o, context);
                    var name = t.Argument(1) as Symbol;
                    if (name == null)
                        throw new ArgumentTypeException("property", "property_name", t.Argument(1), typeof(Symbol));
                    return o.GetPropertyOrField(name.Name);
                }

                case "vector":
                {
                    if (t.Arguments.Length != 3)
                        throw new ArgumentCountException("vector", t.Arguments, "x", "y", "z");
                    return new Vector3(Convert.ToSingle(Eval(t.Argument(0), context)),
                        Convert.ToSingle(Eval(t.Argument(1), context)),
                        Convert.ToSingle(Eval(t.Argument(2), context)));
                }

                case "instance_id":
                {
                    if (t.Arguments.Length != 1)
                        throw new ArgumentCountException("instance_id", t.Arguments, "game_object");
                    var arg = t.Argument(0) as UnityEngine.Object;
                    if (arg == null)
                        throw new ArgumentTypeException("instance_id", "object", t.Argument(0), typeof(UnityEngine.Object));
                    return arg.GetInstanceID();
                }

                default:
                    throw new BadProcedureException(t.Functor, t.Arguments.Length);
            }
        }

        public static object EvalMemberExpression(object obj, object memberExpression, PrologContext context)
        {
            obj = Eval(obj, context);
            memberExpression = Term.Deref(memberExpression);
            var methodCall = memberExpression as Structure;
            if (methodCall != null)
            {
                // Method call
                var args = new object[methodCall.Arity];
                for (var i = 0; i < args.Length; i++)
                {
                    args[i] = Eval(methodCall.Argument(i), context);
                }
                return obj.InvokeMethod(methodCall.Functor.Name, args);
            }
            var propName = memberExpression as Symbol;
            if (propName != null)
            {
                // Field or property reference
                return obj.GetPropertyOrField(propName.Name);
            }
            throw new ArgumentException(
                "Invalid member expression: " + ISOPrologWriter.WriteToString(new Structure(Symbol.Dot, obj, memberExpression)));
        }
    }
}
