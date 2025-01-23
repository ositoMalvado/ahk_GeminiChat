#Requires AutoHotkey v2.0
#DllLoad winhttp.dll

; i used 
;   https://github.com/TheArkive/JXON_ahk2
;   https://github.com/thqby/ahk2_lib/blob/master/JSON.ahk

class GeminiChat {
    __New(apiKey, model := "gemini-1.5-flash", history_file := "") {
        this.apiKey := apiKey
        this.model := model
        this.response := ""
        this.history := []
        this.historyFile := A_ScriptDir . "\" . history_file
        if FileExist(this.historyFile){
            fr := FileRead(this.historyFile)
            fr := Jxon_Load(&fr)
            this.history := fr
        }
        else{
            FileAppend(JSON.stringify(this.history), this.historyFile)
        }
        this.http := ComObject("Msxml2.XMLHTTP.6.0")
        this.http.onreadystatechange := ObjBindMethod(this, "HandleResponse")
    }
    
    BuildPayload(message) {
        payload := {
            contents: [
                {
                    role: "user",
                    parts: [
                        {
                            text: message
                        }
                    ]
                }
            ]
        }
        return JSON.stringify(payload)
    }
    
    SaveMessage(role, content) {
        message := {
            role: role,
            parts: [{text: content}],
        }
        this.history.Push(message)
        if FileExist(this.historyFile)
            FileDelete(this.historyFile)
        FileAppend(JSON.stringify(this.history), this.historyFile)
    }

    SendMessage(message, waitForResponse := true) {
        this.SaveMessage("user", message)
        
        url := "https://generativelanguage.googleapis.com/v1beta/models/" 
             . this.model . ":generateContent?key=" . this.apiKey
        
        this.http.Open("POST", url, true)
        this.http.SetRequestHeader("Content-Type", "application/json")
        payload := this.BuildPayload(message)
        objPayload := Jxon_Load(&payload)
        for x in this.history
            objPayload["contents"].InsertAt(0, x)
        this.http.Send(JSON.stringify(objPayload))
        
        if waitForResponse {
            while this.http.readyState != 4
                Sleep 100
        }
        response := this.response
        response := Jxon_Load(&response)
        this.SaveMessage("model", response["candidates"][1]["content"]["parts"][1]["text"])
        return response["candidates"][1]["content"]["parts"][1]["text"]
    }

    HandleResponse() {
        if (this.http.readyState = 4) { 
            if (this.http.status = 200) {
                this.response := this.http.responseText
            }
            else {
                this.response := "Error: " this.http.status
                MsgBox(this.response, "Error en la solicitud", 16)
            }
        }
    }
}

Jxon_Load(&src, args*) {
	key := "", is_key := false
	stack := [ tree := [] ]
	next := '"{[01234567890-tfn'
	pos := 0
	
	while ( (ch := SubStr(src, ++pos, 1)) != "" ) {
		if InStr(" `t`n`r", ch)
			continue
		if !InStr(next, ch, true) {
			testArr := StrSplit(SubStr(src, 1, pos), "`n")
			
			ln := testArr.Length
			col := pos - InStr(src, "`n",, -(StrLen(src)-pos+1))

			msg := Format("{}: line {} col {} (char {})"
			,   (next == "")      ? ["Extra data", ch := SubStr(src, pos)][1]
			  : (next == "'")     ? "Unterminated string starting at"
			  : (next == "\")     ? "Invalid \escape"
			  : (next == ":")     ? "Expecting ':' delimiter"
			  : (next == '"')     ? "Expecting object key enclosed in double quotes"
			  : (next == '"}')    ? "Expecting object key enclosed in double quotes or object closing '}'"
			  : (next == ",}")    ? "Expecting ',' delimiter or object closing '}'"
			  : (next == ",]")    ? "Expecting ',' delimiter or array closing ']'"
			  : [ "Expecting JSON value(string, number, [true, false, null], object or array)"
			    , ch := SubStr(src, pos, (SubStr(src, pos)~="[\]\},\s]|$")-1) ][1]
			, ln, col, pos)

			return Error(msg, -1, ch)
		}
		
		obj := stack[1]
        is_array := (obj is Array)
		
		if i := InStr("{[", ch) { 
			val := (i = 1) ? Map() : Array()
			
			is_array ? obj.Push(val) : obj[key] := val
			stack.InsertAt(1,val)
			
			next := '"' ((is_key := (ch == "{")) ? "}" : "{[]0123456789-tfn")
		} else if InStr("}]", ch) {
			stack.RemoveAt(1)
            next := (stack[1]==tree) ? "" : (stack[1] is Array) ? ",]" : ",}"
		} else if InStr(",:", ch) {
			is_key := (!is_array && ch == ",")
			next := is_key ? '"' : '"{[0123456789-tfn'
		} else {
			if (ch == '"') {
				i := pos
				while i := InStr(src, '"',, i+1) {
					val := StrReplace(SubStr(src, pos+1, i-pos-1), "\\", "\u005C")
					if (SubStr(val, -1) != "\")
						break
				}
				if !i ? (pos--, next := "'") : 0
					continue

				pos := i 

				val := StrReplace(val, "\/", "/")
				val := StrReplace(val, '\"', '"')
				, val := StrReplace(val, "\b", "`b")
				, val := StrReplace(val, "\f", "`f")
				, val := StrReplace(val, "\n", "`n")
				, val := StrReplace(val, "\r", "`r")
				, val := StrReplace(val, "\t", "`t")

				i := 0
				while i := InStr(val, "\",, i+1) {
					if (SubStr(val, i+1, 1) != "u") ? (pos -= StrLen(SubStr(val, i)), next := "\") : 0
						continue 2

					xxxx := Abs("0x" . SubStr(val, i+2, 4)) 
					if (xxxx < 0x100)
						val := SubStr(val, 1, i-1) . Chr(xxxx) . SubStr(val, i+6)
				}
				
				if is_key {
					key := val, next := ":"
					continue
				}
			} else {
				val := SubStr(src, pos, i := RegExMatch(src, "[\]\},\s]|$",, pos)-pos)
				
                if IsInteger(val)
                    val += 0
                else if IsFloat(val)
                    val += 0
                else if (val == "true" || val == "false")
                    val := (val == "true")
                else if (val == "null")
                    val := ""
                else if is_key {
                    pos--, next := "#"
                    continue
                }
				
				pos += i-1
			}
			
			is_array ? obj.Push(val) : obj[key] := val
			next := obj == tree ? "" : is_array ? ",]" : ",}"
		}
	}
	
	return tree[1]
}

Jxon_Dump(obj, indent:="", lvl:=1) {
	if IsObject(obj) {
        If !(obj is Array || obj is Map || obj is String || obj is Number)
			return Error("Object type not supported.", -1, Format("<Object at 0x{:p}>", ObjPtr(obj)))
		
		if IsInteger(indent)
		{
			if (indent < 0)
				return Error("Indent parameter must be a postive integer.", -1, indent)
			spaces := indent, indent := ""
			
			Loop spaces
				indent .= " "
		}
		indt := ""
		
		Loop indent ? lvl : 0
			indt .= indent
        
        is_array := (obj is Array)
        
		lvl += 1, out := ""
		for k, v in obj {
			if IsObject(k) || (k == "")
				return Error("Invalid object key.", -1, k ? Format("<Object at 0x{:p}>", ObjPtr(obj)) : "<blank>")
			
			if !is_array
				out .= (ObjGetCapacity([k]) ? Jxon_Dump(k) : escape_str(k)) (indent ? ": " : ":") 
			
			out .= Jxon_Dump(v, indent, lvl)
				.  ( indent ? ",`n" . indt : "," )
		}

		if (out != "") {
			out := Trim(out, ",`n" . indent)
			if (indent != "")
				out := "`n" . indt . out . "`n" . SubStr(indt, StrLen(indent)+1)
		}
		
		return is_array ? "[" . out . "]" : "{" . out . "}"
	
    } Else If (obj is Number)
        return obj
    
    Else 
        return escape_str(obj)
	
    escape_str(obj) {
        obj := StrReplace(obj,"\","\\")
        obj := StrReplace(obj,"`t","\t")
        obj := StrReplace(obj,"`r","\r")
        obj := StrReplace(obj,"`n","\n")
        obj := StrReplace(obj,"`b","\b")
        obj := StrReplace(obj,"`f","\f")
        obj := StrReplace(obj,"/","\/")
        obj := StrReplace(obj,'"','\"')
        
        return '"' obj '"'
    }
}

class JSON {
    static null := ComValue(1, 0), true := ComValue(0xB, 1), false := ComValue(0xB, 0)
    static parse(text, keepbooltype := false, as_map := true) {
        keepbooltype ? (_true := this.true, _false := this.false, _null := this.null) : (_true := true, _false := false,
            _null := "")
        as_map ? (map_set := (maptype := Map).Prototype.Set) : (map_set := (obj, key, val) => obj.%key% := val, maptype :=
        Object)
        NQ := "", LF := "", LP := 0, P := "", R := ""
        D := [C := (A := InStr(text := LTrim(text, " `t`r`n"), "[") = 1) ? [] : maptype()], text := LTrim(SubStr(text,
            2), " `t`r`n"), L := 1, N := 0, V := K := "", J := C, !(Q := InStr(text, '"') != 1) ? text := LTrim(text,
                '"') : ""
        loop parse text, '"' {
            Q := NQ ? 1 : !Q
            NQ := Q && RegExMatch(A_LoopField, '(^|[^\\])(\\\\)*\\$')
            if !Q {
                if (t := Trim(A_LoopField, " `t`r`n")) = "," || (t = ":" && V := 1)
                    continue
                else if t && (InStr("{[]},:", SubStr(t, 1, 1)) || A && RegExMatch(t,
                    "m)^(null|false|true|-?\d+(\.\d*(e[-+]\d+)?)?)\s*[,}\]\r\n]")) {
                    loop parse t {
                        if N && N--
                            continue
                        if InStr("`n`r `t", A_LoopField)
                            continue
                        else if InStr("{[", A_LoopField) {
                            if !A && !V
                                return Error("Malformed JSON - missing key.", 0, t)
                            C := A_LoopField = "[" ? [] : maptype(), A ? D[L].Push(C) : map_set(D[L], K, C), D.Has(++L) ?
                                D[L] := C : D.Push(C), V := "", A := Type(C) = "Array"
                            continue
                        } else if InStr("]}", A_LoopField) {
                            if !A && V
                                return Error("Malformed JSON - missing value.", 0, t)
                            else if L = 0
                                return Error("Malformed JSON - to many closing brackets.", 0, t)
                            else C := --L = 0 ? "" : D[L], A := Type(C) = "Array"
                        } else if !(InStr(" `t`r,", A_LoopField) || (A_LoopField = ":" && V := 1)) {
                            if RegExMatch(SubStr(t, A_Index),
                            "m)^(null|false|true|-?\d+(\.\d*(e[-+]\d+)?)?)\s*[,}\]\r\n]", &R) && (N := R.Len(0) - 2, R :=
                            R.1, 1) {
                                if A
                                    C.Push(R = "null" ? _null : R = "true" ? _true : R = "false" ? _false : IsNumber(R) ?
                                        R + 0 : R)
                                else if V
                                    map_set(C, K, R = "null" ? _null : R = "true" ? _true : R = "false" ? _false :
                                        IsNumber(R) ? R + 0 : R), K := V := ""
                                else return Error("Malformed JSON - missing key.", 0, t)
                            } else {
                                ; Added support for comments without '"'
                                if A_LoopField == '/' {
                                    nt := SubStr(t, A_Index + 1, 1), N := 0
                                    if nt == '/' {
                                        if nt := InStr(t, '`n', , A_Index + 2)
                                            N := nt - A_Index - 1
                                    } else if nt == '*' {
                                        if nt := InStr(t, '*/', , A_Index + 2)
                                            N := nt + 1 - A_Index
                                    } else nt := 0
                                    if N
                                        continue
                                }
                                return Error("Malformed JSON - unrecognized character.", 0, A_LoopField " in " t)
                            }
                        }
                    }
                } else if A || InStr(t, ':') > 1
                    return Error("Malformed JSON - unrecognized character.", 0, SubStr(t, 1, 1) " in " t)
            } else if NQ && (P .= A_LoopField '"', 1)
                continue
            else if A
                LF := P A_LoopField, C.Push(InStr(LF, "\") ? UC(LF) : LF), P := ""
            else if V
                LF := P A_LoopField, map_set(C, K, InStr(LF, "\") ? UC(LF) : LF), K := V := P := ""
            else
                LF := P A_LoopField, K := InStr(LF, "\") ? UC(LF) : LF, P := ""
        }
        return J
        UC(S, e := 1) {
            static m := Map('"', '"', "a", "`a", "b", "`b", "t", "`t", "n", "`n", "v", "`v", "f", "`f", "r", "`r")
            local v := ""
            loop parse S, "\"
                if !((e := !e) && A_LoopField = "" ? v .= "\" : !e ? (v .= A_LoopField, 1) : 0)
                    v .= (t := m.Get(SubStr(A_LoopField, 1, 1), 0)) ? t SubStr(A_LoopField, 2) :
                        (t := RegExMatch(A_LoopField, "i)^(u[\da-f]{4}|x[\da-f]{2})\K")) ?
                            Chr("0x" SubStr(A_LoopField, 2, t - 2)) SubStr(A_LoopField, t) : "\" A_LoopField,
                    e := A_LoopField = "" ? e : !e
            return v
        }
    }

    static stringify(obj, expandlevel := unset, space := "  ") {
        expandlevel := IsSet(expandlevel) ? Abs(expandlevel) : 10000000
        return Trim(CO(obj, expandlevel))
        CO(O, J := 0, R := 0, Q := 0) {
            static M1 := "{", M2 := "}", S1 := "[", S2 := "]", N := "`n", C := ",", S := "- ", E := "", K := ":"
            if (OT := Type(O)) = "Array" {
                D := !R ? S1 : ""
                for key, value in O {
                    F := (VT := Type(value)) = "Array" ? "S" : InStr("Map,Object", VT) ? "M" : E
                    Z := VT = "Array" && value.Length = 0 ? "[]" : ((VT = "Map" && value.count = 0) || (VT = "Object" &&
                        ObjOwnPropCount(value) = 0)) ? "{}" : ""
                    D .= (J > R ? "`n" CL(R + 2) : "") (F ? (%F%1 (Z ? "" : CO(value, J, R + 1, F)) %F%2) : ES(value)) (
                        OT = "Array" && O.Length = A_Index ? E : C)
                }
            } else {
                D := !R ? M1 : ""
                for key, value in (OT := Type(O)) = "Map" ? (Y := 1, O) : (Y := 0, O.OwnProps()) {
                    F := (VT := Type(value)) = "Array" ? "S" : InStr("Map,Object", VT) ? "M" : E
                    Z := VT = "Array" && value.Length = 0 ? "[]" : ((VT = "Map" && value.count = 0) || (VT = "Object" &&
                        ObjOwnPropCount(value) = 0)) ? "{}" : ""
                    D .= (J > R ? "`n" CL(R + 2) : "") (Q = "S" && A_Index = 1 ? M1 : E) ES(key) K (F ? (%F%1 (Z ? "" :
                        CO(value, J, R + 1, F)) %F%2) : ES(value)) (Q = "S" && A_Index = (Y ? O.count : ObjOwnPropCount(
                            O)) ? M2 : E) (J != 0 || R ? (A_Index = (Y ? O.count : ObjOwnPropCount(O)) ? E : C) : E)
                    if J = 0 && !R
                        D .= (A_Index < (Y ? O.count : ObjOwnPropCount(O)) ? C : E)
                }
            }
            if J > R
                D .= "`n" CL(R + 1)
            if R = 0
                D := RegExReplace(D, "^\R+") (OT = "Array" ? S2 : M2)
            return D
        }
        ES(S) {
            switch Type(S) {
                case "Float":
                    if (v := '', d := InStr(S, 'e'))
                        v := SubStr(S, d), S := SubStr(S, 1, d - 1)
                    if ((StrLen(S) > 17) && (d := RegExMatch(S, "(99999+|00000+)\d{0,3}$")))
                        S := Round(S, Max(1, d - InStr(S, ".") - 1))
                    return S v
                case "Integer":
                    return S
                case "String":
                    S := StrReplace(S, "\", "\\")
                    S := StrReplace(S, "`t", "\t")
                    S := StrReplace(S, "`r", "\r")
                    S := StrReplace(S, "`n", "\n")
                    S := StrReplace(S, "`b", "\b")
                    S := StrReplace(S, "`f", "\f")
                    S := StrReplace(S, "`v", "\v")
                    S := StrReplace(S, '"', '\"')
                    return '"' S '"'
                default:
                    return S == this.true ? "true" : S == this.false ? "false" : "null"
            }
        }
        CL(i) {
            loop (s := "", space ? i - 1 : 0)
                s .= space
            return s
        }
    }
}
