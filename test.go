package main

import (
    "fmt"
    "strings"
    "strconv"
    "math"
    "math/big"
	"container/list"
)

const (
    WHITESPACE =
        "\u0009\u000A\u000B\u000C\u000D\u0020" +
        "\u0085\u00A0\u1680\u180E\u2000\u2001" +
        "\u2002\u2003\u2004\u2005\u2006\u2007" +
        "\u2008\u2009\u200A\u2028\u2029\u202F" +
        "\u205F\u3000"
    RESERVED = WHITESPACE + ".;{}()\""
    DIGITS = "0123456789"
    T_LPAR = iota
    T_RPAR = iota
    T_WHITESPACE = iota
    T_INTEGER = iota
    T_REAL = iota
    T_STRING = iota
    T_NAME = iota
    T_COMMENT = iota
    INVALID = iota
)

func TerminalWhitespace(t string) (s string, ok bool) {
    runes := strings.Split(t, "")
    for i := range runes {
        if strings.Index(WHITESPACE, runes[i]) == -1 {
            return "", false
        }
    }
    return "", true
}

func TerminalComment(t string) (s string, ok bool) {
    runes := strings.Split(t, "")
    if runes[0] == "{" && runes [len(runes) - 1] == "}" {
        return t, true
    }
    if runes[0] == ";" && runes [len(runes) - 1] == "\n" {
        return t, true
    }
    return "", false
}

func TerminalString(t string) (s string, ok bool) {
    if !strings.HasPrefix(t, "\"") {
        return "", false
    }
    _, err := strconv.Unquote(t)
    if err == nil {
        return t, true
    }
    return "", false
}

func TerminalInteger(t string) (s string, ok bool) {
    if _, err := strconv.ParseInt(t, 0, 64); err == nil {
        return t, true
    }
    return "", false
}

func TerminalReal(t string) (s string, ok bool) {
    if _, err := strconv.ParseFloat(t, 64); err == nil {
        return t, true
    }
    return "", false
}

func TerminalName(t string) (s string, ok bool) {
    if _, ok := TerminalWhitespace(t); ok {
        return "", false
    }
    if _, ok := TerminalComment(t); ok {
        return "", false
    }
    if _, ok := TerminalString(t); ok {
        return "", false
    }
    if _, ok := TerminalInteger(t); ok {
        return "", false
    }
    if _, ok := TerminalReal(t); ok {
        return "", false
    }
    runes := strings.Split(t, "")
    for i := range runes {
        if strings.Index(RESERVED, runes[i]) != -1 {
            return "", false
        }
    }
    return t, true
}

func GetToken(token string) (s string, ok bool) {
    getToken := func (token string) (s string, ok bool) {
        if token == "(" || token == ")" {
            return token, true
        }
        if t, ok := TerminalWhitespace(token); ok {
            return t, true
        }
        if t, ok := TerminalComment(token); ok {
            return t, true
        }
        if t, ok := TerminalString(token); ok {
            return t, true
        }
        if t, ok := TerminalInteger(token); ok {
            return t, true
        }
        if t, ok := TerminalReal(token); ok {
            return t, true
        }
        if t, ok := TerminalName(token); ok {
            return t, true
        }
        return "", false
    }
    if strings.HasPrefix("#", token) {
        if _, ok := getToken(token); ok {
            return token, true
        }
    }
    return getToken(token)
}

func Tokenize(code string) []string {
    runes := strings.Split(code, "")
    token, tokens := "", []string{}
    for len(runes) != 0 {
        token, runes = token + runes[0], runes[1:]
        if t, ok := GetToken(token); ok {
            if _, ok := GetToken(token + (append(runes, "\x00"))[0]); ok {
                continue
            }
            if _, ok := TerminalReal(t); ok && (runes[0] == "e" || runes[0] == "E") {
                continue
            }
            tokens = append(tokens, t)
            token = ""
        }
    }
    return tokens
}

func listAppend(m *list.List, x interface{}) {
	m.PushBack(x)
}

func listPop(m *list.List) {
	m.Remove(m.Back())
}

func ParseTokens(t []string) *list.List {
    tokens := new(list.List)
    for i := range t {
        tokens.PushBack(t[i])
    }
    n := 0
	s, o := new(list.List), new(list.List)
	for e := tokens.Front(); e != nil; e = e.Next() {
		if e.Value.(string) == "(" {
            n++
			listAppend(s, new(list.List))
			listAppend(o, s)
			s = s.Back().Value.(*list.List)
		} else if e.Value.(string) == ")" {
            n--
			s = o.Back().Value.(*list.List)
			listPop(o)
		} else {
			listAppend(s, e.Value.(string))
		}
	}
    if n != 0 {
        panic("unbalanced parantheses")
    }
	return s
}

func Slicify(l *list.List) []interface{} {
    result := []interface{}{}
	for e := l.Front(); e != nil; e = e.Next() {
        if s, ok := e.Value.(string); ok {
            if len(s) > 0 {
                result = append(result, s)
            }
        } else {
            result = append(result, Slicify(e.Value.(*list.List)))
        }
    }
    return result
}

func Ast(code string) []interface{} {
    l := ParseTokens(Tokenize(code))
    return Slicify(l)
}

type Stack []interface{}
type Local []interface{}
var Instructions map[string]int

func numeric_binary(stack Stack,
        func_int (func (*big.Int, *big.Int) interface{}),
        func_float (func (float64, float64) interface{})) Stack {
    x_ := stack[len(stack) - 2]
    y_ := stack[len(stack) - 1]
    stack = stack[:len(stack) - 2]
    var result interface{}
    if x, ok := x_.(*big.Int); ok {
        if y, ok := y_.(*big.Int); ok {
            result = new(big.Int)
            result = func_int(x, y)
        } else if y, ok := y_.(float64); ok {
            xf, _ := strconv.ParseFloat(x.String(), 64)
            result = func_float(xf, y)
        } else {
            panic("type error")
        }
    } else if x, ok := x_.(float64); ok {
        if y, ok := y_.(*big.Int); ok {
            yf, _ := strconv.ParseFloat(y.String(), 64)
            result = func_float(x, yf)
        } else if y, ok := y_.(float64); ok {
            result = func_float(x, y)
        } else {
            panic("type error")
        }
    } else {
        panic("type error")
    }
    return append(stack, result)
}

func evaluate(code [][]int, names map[int]interface{}, stack Stack, local Local) (Stack, Local) {
    ip := 0
    ictr := 0
    calls := []int{}
    outer := []*Local{&local}
    var skip bool
    for ip < len(code) {
        local = *outer[len(outer) - 1]
        inst, data := code[ip][0], code[ip][1]
        /*for i := range Instructions {
            if Instructions[i] == inst {
                fmt.Println(i, names[data], stack)//, data, stack, names)
            }
        }*/
        ip++
        if skip {
            ip++
            skip = false
        }
        ictr += 1
        if inst == -1 {
        } else if inst == 1 /* PRINT */ {
            x := stack[len(stack) - 1]
            stack = stack[:len(stack) - 1]
            fmt.Println(">>>", x)
            stack = append(stack, big.NewInt(0))
        } else if inst == 2 /* STO */ {
            if len(local) <= data {
                ext := make(Local, data - len(local))
                local = append(local, ext...)
            }
            local[data] = stack[len(stack) - 1]
            if code[ip + 1][0] != 3 {
                stack = stack[:len(stack) - 1]
            } else {
                ip++
            }
        } else if inst == 3 /* RCL */ {
            if data >= len(local) {
                panic("illegal memory")
            }
            x := local[data]
            if x == nil {
                panic(fmt.Sprintf("undefined: %v", names[data]))
            }
            stack = append(stack, x)
        } else if inst == 4 /* STORCL */ {
            if len(local) <= data {
                ext := make(Local, data - len(local))
                local = append(local, ext...)
            }
            local[data] = stack[len(stack) - 1]
        } else if inst == 8 /* EVAL */ {
            stack = append(stack, names[data])
        } else if inst == 9 /* POP */ {
            stack = stack[:len(stack) - 1]
        } else if inst == 11 /* .SUB */ {
            if data == -1 {
                stack = append(stack, ip)
            } else {
                local[data] = ip
            }
            i := 1
            for i != 0 {
                inst := code[ip][0]
                ip++
                if inst == 11 /* .SUB */ {
                    i++
                } else if inst == 12 /* .END */ {
                    i--
                }
            }
        } else if inst == 30 /* args */ {
            x := stack[len(stack) - 1].([]interface{})
            stack = stack[:len(stack) - 1]
            for i := range x {
                stack = append(stack, x[len(x) - i - 1])
            }
        } else if inst == 31 /* LIST */ {
            args := []interface{}{}
            for i := 0; i < data; i++ {
                x := stack[len(stack) - data + i]
                args = append(args, x)
            }
            for i := 0; i < data; i++ {
                stack = stack[:len(stack) - 1]
            }
            stack = append(stack, args)
        } else if inst == 32 /* CAR */ {
            x_ := stack[len(stack) - 1]
            stack = stack[:len(stack) - 1]
            if x, ok := x_.([]interface{}); ok {
                if len(x) > 0 {
                    stack = append(stack, x[0])
                } else {
                    panic("cannot take car of the empty list")
                }
            } else {
                panic("not a list")
            }
        } else if inst == 33 /* CDR */ {
            x_ := stack[len(stack) - 1]
            stack = stack[:len(stack) - 1]
            if x, ok := x_.([]interface{}); ok {
                if len(x) > 0 {
                    stack = append(stack, x[1:])
                } else {
                    panic("cannot take cdr of the empty list")
                }
            } else {
                panic("not a list")
            }
        } else if inst == 12 /* .END */ {
            outer = outer[:len(outer) - 1]
            ip = calls[len(calls) - 1]
            calls = calls[:len(calls) - 1]
        } else if inst == 20 /* CALL */ {
            calls = append(calls, ip)
            cp := make(Local, len(local))
            copy(cp, local)
            outer = append(outer, &cp)
            x := stack[len(stack) - 1]
            stack = stack[:len(stack) - 1]
            ip = x.(int)
        } else if inst == 21 /* TAIL */ {
            x := stack[len(stack) - 1]
            stack = stack[:len(stack) - 1]
            ip = x.(int)
        } else if inst == 50 /* ADD */ {
            x := stack[len(stack) - 2]
            y := stack[len(stack) - 1]
            if x, ok := x.([]interface{}); ok {
                if y, ok := y.([]interface{}); ok {
                    stack = stack[:len(stack) - 2]
                    stack = append(stack, append(x, y...))
                } else {
                    panic("type error")
                }
            } else {
                stack = numeric_binary(stack,
                    func (a, b *big.Int) interface{} {
                        return new(big.Int).Add(a, b)
                    }, func (a, b float64) interface{} {
                        return a + b
                    })
            }
        } else if inst == 51 /* SUB */ {
            stack = numeric_binary(stack,
                func (a, b *big.Int) interface{} {
                    return new(big.Int).Sub(a, b)
                }, func (a, b float64) interface{} {
                    return a - b
                })
        } else if inst == 52 /* MUL */ {
            stack = numeric_binary(stack,
                func (a, b *big.Int) interface{} {
                    return new(big.Int).Mul(a, b)
                }, func (a, b float64) interface{} {
                    return a * b
                })
        } else if inst == 53 /* DIV */ {
            stack = numeric_binary(stack,
                func (a, b *big.Int) interface{} {
                    return new(big.Int).Div(a, b)
                }, func (a, b float64) interface{} {
                    return a / b
                })
        } else if inst == 54 /* POW */ {
            stack = numeric_binary(stack,
                func (a, b *big.Int) interface{} {
                    return new(big.Int).Exp(a, b, nil)
                }, func (a, b float64) interface{} {
                    return math.Pow(a, b)
                })
        } else if inst == 60 /* EQ */ {
            stack = numeric_binary(stack,
                func (a, b *big.Int) interface{} {
                    if a.Cmp(b) == 0 {
                        return big.NewInt(int64(1))
                    }
                    return 0.0
                }, func (a, b float64) interface{} {
                    if a == b {
                        return 1.0
                    }
                    return 0.0
                })
        } else if inst == 61 /* GT */ {
            stack = numeric_binary(stack,
                func (a, b *big.Int) interface{} {
                    if a.Cmp(b) == 1 {
                        return 1.0
                    }
                    return big.NewInt(int64(0))
                }, func (a, b float64) interface{} {
                    if a > b {
                        return 1.0
                    }
                    return 0.0
                })
        } else if inst == 70 /* .IF */ {
            x := stack[len(stack) - 1]
            stack = stack[:len(stack) - 1]
            b := true
            if x_, ok := x.(float64); ok {
                if x_ == 0.0 {
                    b = false
                }
            } else if x_, ok := x.(*big.Int); ok {
                if x_.Cmp(big.NewInt(0)) == 0 {
                    b = false
                }
            } else if x_, ok := x.([]interface{}); ok {
                if len(x_) == 0 {
                    b = false
                }
            }
            i := 1
            else_case := -1
            ip_ := ip
            for i != 0 {
                inst := code[ip_][0]
                ip_++
                if inst == 70 /* .IF */ {
                    i++
                } else if inst == 71 /* .ELSE */ {
                    else_case = ip_
                } else if inst == 72 /* .ENDIF */ {
                    i--
                }
            }
            if !b {
                ip = else_case
            }
        } else if inst == 71 /* .ELSE */ {
            i := 1
            for i != 0 {
                inst := code[ip][0]
                ip++
                if inst == 70 /* .IF*/ {
                    i++
                } else if inst == 72 /* .ENDIF */ {
                    i--
                }
            }
        } else if inst == 72 /* .ENDIF */ {
        } else {
            panic(fmt.Sprintf("error %v", inst))
        }
    }
    fmt.Println("executed", ictr, "bytecode instructions")
    return stack, local
}

func translate(code string, index int) ([][]int, map[int]interface{}, int) {
    Instructions = map[string]int{
        "#": -1,
        "print": 1,
        "sto": 2,
        "rcl": 3,
        "storcl": 4,
        "eval": 8,
        "pop": 9,
        ".sub": 11,
        ".end": 12,
        "call": 20,
        "tail": 21,
        "args": 30,
        "list": 31,
        "car": 32,
        "cdr": 33,
        "add": 50,
        "sub": 51,
        "mul": 52,
        "div": 53,
        "pow": 54,
        "eq": 60,
        "gt": 61,
        ".if": 70,
        ".else": 71,
        ".endif": 72,
    }
    names := map[int]interface{}{}
    vars := map[interface{}]int{}
    m := [][]int{}
    c := strings.Split(code, "\n")
    for i := range c {
        j := strings.Trim(c[i], " ")
        if len(j) == 0 {
            continue
        }
        inst := strings.Split(j, " ")[0]
        data := strings.Trim(j[len(inst):], " ")
        k := []int{}
        if i, ok := Instructions[inst]; ok {
            k = append(k, i)
        } else {
            panic("invalid instruction: " + inst)
        }
        if inst == "eval" {
            k = append(k, index)
            if d, err := strconv.ParseInt(data, 10, 64); err == nil {
                names[index] = big.NewInt(d)
            } else if d, err := strconv.ParseFloat(data, 64); err == nil {
                names[index] = float64(d)
            } else if d, err := strconv.Unquote(data); err == nil {
                names[index] = d
            } else {
                panic("error parsing constant: " + data)
            }
            index++
        } else if inst == "list" {
            if i, err := strconv.ParseInt(data, 10, 64); err == nil {
                k = append(k, int(i))
            } else {
                panic("error parsing constant: " + data)
            }
        } else if len(data) > 0 {
            if d, ok := vars[data]; ok {
                k = append(k, d)
            } else {
                k = append(k, index)
                names[index] = data
                vars[data] = index
                index++
            }
        } else {
            k = append(k, -1)
        }
        m = append(m, k)
    }
    return m, names, index
}

func Eval(atom interface{}, tail, define string) string {
    code := ""
    if expr, ok := atom.(string); ok && len(expr) > 0 {
        if t, ok := TerminalString(expr); ok {
            return "eval " + t + "\n"
        }
        if t, ok := TerminalInteger(expr); ok {
            return "eval " + t + "\n"
        }
        if t, ok := TerminalReal(expr); ok {
            return "eval " + t + "\n"
        }
        if t, ok := TerminalName(expr); ok {
            return "rcl " + t + "\n"
        }
        if _, ok := TerminalComment(expr); ok {
            return ""
        }
    } else if list, ok := atom.([]interface{}); ok && len(list) > 0 {
        if f, ok := list[0].(string); ok && f == "lambda" {
            args := list[1]
            body := list[2:]
            if len(body) > 0 {
                body_head := body[:len(body) - 1]
                body_tail := body[len(body) - 1]
                code += ".sub \n"
                if a, ok := args.([]interface{}); ok {
                    code += fmt.Sprintf("  args %d\n", len(a))
                    for i := range(a) {
                        code += "  sto " + a[i].(string) + "\n"
                    }
                } else {
                        code += "  sto " + args.(string) + "\n"
                }
                for i := range body_head {
                    c := strings.Split(Eval(body[i], "", ""), "\n")
                    for j := range c {
                        code += "  " + c[j] + "\n"
                    }
                    code += "  pop\n"
                }
                c := strings.Split(Eval(body_tail, define, ""), "\n")
                for j := range c {
                    code += "  " + c[j] + "\n"
                }
                code += ".end"
                return code + "\n"
            } else {
                code += ".sub\n"
                if a, ok := args.([]interface{}); ok {
                    code += fmt.Sprintf("  args %d\n", len(a))
                    code += fmt.Sprintf("  list %d\n", len(a))
                }
                code += ".end\n"
                return code
            }
        } else if f, ok := list[0].(string); ok && f == "if" {
            cond := Eval(list[1], "", "")
            true_case := Eval(list[2], tail, "")
            false_case := Eval(list[3], tail, "")
            code = cond + ".if\n"
            c := strings.Split(true_case, "\n")
            for j := range c {
                code += "  " + c[j] + "\n"
            }
            code += ".else\n"
            d := strings.Split(false_case, "\n")
            for j := range d {
                code += "  " + d[j] + "\n"
            }
            code += ".endif\n"
            return code
        } else if f, ok := list[0].(string); ok && f == "define" {
            code := Eval(list[2], "", list[1].(string))
            code += "storcl " + list[1].(string) + "\n"
            return code
        } else if f, ok := list[0].(string); ok && f == "list" {
            args := list[1:]
            for i := range args {
                code += Eval(args[i], "", "")
            }
            code += fmt.Sprintf("list %d\n", len(args))
            return code
        } else if len(list) > 0 {
            builtins2 := map[string]string{
                ">": "gt",
                "=": "eq",
                "+": "add",
                "-": "sub",
                "*": "mul",
                "/": "div",
                "^": "pow",}
            builtins1 := map[string]string{
                "print": "print",
                "car": "car",
                "cdr": "cdr",}
            args := list[1:]
            for i := range args {
                code += Eval(args[i], "", "")
            }
            if n, ok := list[0].(string); ok {
                if i, ok := builtins2[n]; ok {
                    if len(args) != 2 {
                        panic("invalid number of arguments")
                    }
                    return code + i + "\n"
                } else if i, ok := builtins1[n]; ok {
                    if len(args) != 1 {
                        panic("invalid number of arguments")
                    }
                    return code + i + "\n"
                }
            }
            code += fmt.Sprintf("list %d\n", len(args))
            code += Eval(list[0], "", "")
            if len(tail) > 0 && list[0] == tail {
                code += "tail\n"
                return code
            } else {
                code += "call\n"
                return code
            }
        } else {
            panic(fmt.Sprintf("not an expression: %v", list))
        }
    }
    panic(fmt.Sprintf("empty: %v", atom))
}

func main() {
    a := `

    (print (+ (list 1 2 3) (list 4 5)))


(print (^ 2.0 16.0))
(print ((lambda xs) 1 2 3))

(define m (list 1 2 3))
(print m)
(print (car m))
(print (cdr m))

(define product (lambda (m)
    (if m
            (* (car m) (product (cdr m)))
        1)))

(print (product (list 1 2 3 4)))


    (define M (lambda (n)
        (if (> n 100.0)
                (- n 10.0)
            (M (M (+ n 11.0))))))

    (define loop (lambda (n)
        (print (M 99.0))
        (if (> n 1.0) (loop (- n 1.0)) 0.0)))

    (loop 16.0)

`
    code := ""
    ast := Ast(a)
    for i := range ast {
        code += Eval(ast[i], "", "")
    }
    fmt.Println(code)
    //return
    c, names, varcount := translate(`

    ` +  code, 0)
    stack, local := Stack{}, make(Local, varcount)
    stack, local = evaluate(c, names, stack, local)
}
