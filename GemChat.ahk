#Requires AutoHotkey v2.0
#Include jsongo.ahk
class GeminiChat {
    __New(apiKey, model := "gemini-1.5-flash", history_file := "history.json") {
        this.apiKey := apiKey
        this.model := model
        this.response := ""
        this.history := []
        this.historyFile := A_ScriptDir . "\" . history_file
        if FileExist(this.historyFile){
            fr := FileRead(this.historyFile)
            fr := jsongo.Parse(fr)
            this.history := fr
        }
        else{
            FileAppend(jsongo.Stringify(this.history), this.historyFile)
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
        return jsongo.Stringify(payload)
    }
    
    SaveMessage(role, content) {
        message := {
            role: role,
            parts: [{text: content}],
        }
        this.history.Push(message)
        if FileExist(this.historyFile)
            FileDelete(this.historyFile)
        FileAppend(jsongo.Stringify(this.history), this.historyFile)
    }

    SendMessage(message, waitForResponse := true) {
        this.SaveMessage("user", message)
        
        url := "https://generativelanguage.googleapis.com/v1beta/models/" 
             . this.model . ":generateContent?key=" . this.apiKey
        
        this.http.Open("POST", url, true)
        this.http.SetRequestHeader("Content-Type", "application/json")
        payload := this.BuildPayload(message)
        objPayload := jsongo.Parse(payload)
        for x in this.history
            objPayload["contents"].InsertAt(0, x)
        this.http.Send(jsongo.Stringify(objPayload))
        
        if waitForResponse {
            while this.http.readyState != 4
                Sleep 100
        }
        response := this.response
        response := jsongo.Parse(response)
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
