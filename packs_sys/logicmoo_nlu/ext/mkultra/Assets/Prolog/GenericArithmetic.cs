// Copyright 2007, 2008, 2009, 2010, 2011 Ian Horswill
// This file is part of Twig.
//
// Twig is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as 
// published by the Free Software Foundation, either version 3 of
//  the License, or (at your option) any later version.
//
// Twig is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Twig.  If not, see <http://www.gnu.org/licenses/>.

using System;
using UnityEngine;

namespace Prolog
{
    public static class GenericArithmetic
    {
        public static object Add(params object[] args)
        {
            int iResult=0;
            int i = 0;
            for (;i<args.Length; i++) {
                object a = args[i];
                if (a is int)
                    iResult += (int)a;
                else
                    goto floatCase;
            }
            return iResult;

        floatCase:
            float fResult = iResult;
            for (; i < args.Length; i++)
                if (Convert.GetTypeCode(args[i]) == TypeCode.Object)
                    goto objectCase;
                else
                    fResult += Convert.ToSingle(args[i]);
            return fResult;

        objectCase:
            if (i != 0)
                throw new ArithmeticException("Can't add numbers and complex objects");
            if (args[0] is Vector2)
            {
                Vector2 result = Vector2.zero;
                for (; i < args.Length; i++)
                    result += (Vector2)args[i];
                return result;
            }
            if (args[0] is Vector3)
            {
                Vector3 result = Vector3.zero;
                for (; i < args.Length; i++)
                    result += (Vector3)args[i];
                return result;
            }
#if MatrixArith
            if (args[0] is Matrix4x4)
            {
                var result = new Matrix4x4();
                for (; i < args.Length; i++)
                    result += (Matrix4x4)args[i];
                return result;
            }
#endif
            throw new MissingMethodException("Cannot perform addition on specified arguments.");
        }

        public static object Multiply(params object[] args)
        {
            int iResult = 1;
            int i = 0;
            for (; i < args.Length; i++)
            {
                object a = args[i];
                if (a is int)
                    iResult *= (int)a;
                else
                    goto floatCase;
            }
            return iResult;

        floatCase:
            float fResult = iResult;
            for (; i < args.Length; i++)
                if (Convert.GetTypeCode(args[i]) == TypeCode.Object)
                    goto objectCase;
                else
                    fResult *= Convert.ToSingle(args[i]);
            return fResult;

        objectCase:
            if (args[i] is Vector2)
            {
                Vector2 result = fResult*(Vector2)args[i++];
                for (; i < args.Length; i++)
                    result *= Convert.ToSingle(args[i]);
                return result;
            }
            if (args[i] is Vector3)
            {
                Vector3 result = fResult*(Vector3)args[i++];
                for (; i < args.Length; i++)
                    result *= Convert.ToSingle(args[i]);
                return result;
            }
#if MatrixArith
            if (args[i] is Matrix4x4)
            {
                Matrix4x4 result = fResult*(Matrix4x4)args[i++];
                for (; i < args.Length; i++)
                    if (args[i] is Matrix4x4)
                        result *= result;
                    else
                        result *= Convert.ToSingle(args[i]);
                return result;
            }
#endif
            throw new MissingMethodException("Cannot perform addition on specified arguments.");
        }

        public static object Subtract(params object[] args)
        {
            int argCount = args.Length;
            if (argCount == 0)
                throw new ArithmeticException("- cannot be called without arguments");
            object firstArg = args[0];
            if (argCount == 1)
                switch (Convert.GetTypeCode(firstArg))
                {
                    case TypeCode.Int32:
                        return -(int)firstArg;

                    case TypeCode.Single:
                    case TypeCode.Double:
                        return -Convert.ToSingle(firstArg);

                    case TypeCode.Object:
                        return Negate(firstArg);

                    default:
                        throw new ArithmeticException("Invalid argument to subtraction");
                }
            int i;
            switch (Convert.GetTypeCode(firstArg))
            {
                case TypeCode.Int32: {
                    var iResult = (int)firstArg;
                    for (i = 1; i < args.Length; i++)
                        if (args[i] is int)
                            iResult -= (int)args[i];
                        else
                            goto floatCase;
                    return iResult;
                    floatCase:
                    float fResult = iResult;
                    for (; i < args.Length; i++)
                        fResult -= Convert.ToSingle(args[i]);
                    return fResult;
                }

                case TypeCode.Single:
                case TypeCode.Double:
                    float ffResult = Convert.ToSingle(firstArg);
                    for (i=1; i < args.Length; i++)
                        ffResult -= Convert.ToSingle(args[i]);
                    return ffResult;

                case TypeCode.Object:
                    if (firstArg is Vector2)
                    {
                        var result = (Vector2)firstArg;
                        for (i = 1; i < args.Length; i++)
                            result -= (Vector2)args[i];
                        return result;
                    }
                    if (firstArg is Vector3)
                    {
                        var result = (Vector3)firstArg;
                        for (i = 1; i < args.Length; i++)
                            result -= (Vector3)args[i];
                        return result;
                    }
#if MatrixArith
                    if (firstArg is Matrix4x4)
                    {
                        Matrix4x4 result = (Matrix4x4)firstArg;
                        for (i = 1; i < args.Length; i++)
                            result -= (Matrix4x4)args[i];
                        return result;
                    }
#endif
                    throw new ArithmeticException("Invalid argument to subtraction");

                default:
                    throw new ArithmeticException("Invalid argument to subtraction");
            }
        }

        static object Negate(object x)
        {
            if (x is Vector2)
                return -(Vector2)x;
            if (x is Vector3)
                return -(Vector3)x;
#if MatrixArith
            if (x is Matrix4x4)
                return -(Matrix4x4)x;
#endif
            throw new ArithmeticException("Invalid argument to -");
        }

        public static object Divide(params object[] args)
        {
            int argCount = args.Length;
            int i;
            if (argCount == 0)
                throw new ArithmeticException("/ cannot be called without arguments");
            object firstArg = args[0];
            if (argCount == 1)
                switch (Convert.GetTypeCode(firstArg))
                {
                    case TypeCode.Int32:
                    case TypeCode.Single:
                    case TypeCode.Double:
                        return 1/Convert.ToSingle(firstArg);

                    default:
                        throw new ArithmeticException("Invalid argument to division");
                }
            switch (Convert.GetTypeCode(firstArg))
            {
                case TypeCode.Int32:
                case TypeCode.Single:
                case TypeCode.Double:
                    float ffResult = Convert.ToSingle(firstArg);
                    for (i = 1; i < args.Length; i++)
                        ffResult /= Convert.ToSingle(args[i]);
                    return ffResult;

                case TypeCode.Object:
                    if (firstArg is Vector2)
                    {
                        var result = (Vector2)firstArg;
                        for (i = 1; i < args.Length; i++)
                            result /= Convert.ToSingle(args[i]);
                        return result;
                    }
                    if (firstArg is Vector3)
                    {
                        var result = (Vector3)firstArg;
                        for (i = 1; i < args.Length; i++)
                            result /= Convert.ToSingle(args[i]);
                        return result;
                    }
#if MatrixArith
                    if (firstArg is Matrix4x4)
                    {
                        Matrix4x4 result = (Matrix4x4)firstArg;
                        for (i = 1; i < args.Length; i++)
                            result /= Convert.ToSingle(args[i]);
                        return result;
                    }
#endif
                    throw new ArithmeticException("Invalid argument to subtraction");

                default:
                    throw new ArithmeticException("Invalid argument to subtraction");
            }
        }

        public static object Min(params object[] args) {
            if (args.Length == 0)
                throw new ArgumentException("min requires at least one argument");

            int iResult = 0;
            int i = 0;
            if (!(args[0] is int))
                goto floatCase;

            iResult = (int)args[i++];

            for (; i < args.Length; i++)
            {
                object a = args[i];
                if (a is int)
                {
                    var ai = (int)a;
                    if (ai<iResult)
                        iResult = ai;
                }
                else
                    goto floatCase;
            }
            return iResult;

        floatCase:
            float fResult = (i > 0) ? iResult : Convert.ToSingle(args[i++]);
            for (; i < args.Length; i++)
            {
                float f = Convert.ToSingle(args[i]);
                if (f < fResult)
                    fResult = f;
            }
            return fResult;
        }

        public static object Max(params object[] args)
        {
            if (args.Length == 0)
                throw new ArgumentException("max requires at least one argument");

            int iResult = 0;
            int i = 0;
            if (!(args[0] is int))
                goto floatCase;

            iResult = (int)args[i++];

            for (; i < args.Length; i++)
            {
                object a = args[i];
                if (a is int)
                {
                    var ai = (int)a;
                    if (ai > iResult)
                        iResult = ai;
                }
                else
                    goto floatCase;
            }
            return iResult;

        floatCase:
            float fResult = (i > 0) ? iResult : Convert.ToSingle(args[i++]);
            for (; i < args.Length; i++)
            {
                float f = Convert.ToSingle(args[i]);
                if (f > fResult)
                    fResult = f;
            }
            return fResult;
        }
    }
}
