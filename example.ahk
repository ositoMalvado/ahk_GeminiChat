#Requires AutoHotkey v2.0
#Include GemChat.ahk

; Get your API_KEY for free here: https://aistudio.google.com/app/apikey?hl=es-419
; Jsongo from https://github.com/GroggyOtter/jsongo_AHKv2

Class ChatGui{
    __New(){
        this.gem_chat := GeminiChat(FileRead("api_key"))
        this.MyGui := Gui()
        this.MyGui.Title := "Gemini Chat"
        this.chat_visualizer := this.MyGui.AddEdit("+ReadOnly +Multi w580 h380")
        this.chat_input := this.MyGui.AddEdit("+Multi vChatInput w500 h80 Section")
        this.button_send := this.MyGui.AddButton("yp w80 h40 Disabled", "Send")
        this.button_send.OnEvent("Click", (*) => (this.events("send")))
        this.button_clear := this.MyGui.AddButton("w80 h40 Disabled", "Clear")
        this.button_clear.OnEvent("Click", (*) => (this.events("clear")))
        this.chat_input.OnEvent("Change", (*) => (this.events("edit")))
        this.MyGui.Show("xCenter yCenter w600 h480")
        this.chat_input.Focus()
    }

    events(data:="", *){
        if data == "clear"{
            this.chat_input.Text := ""
        } else if data == "send"{
            this.button_send.Opt("+Disabled")
            this.button_clear.Opt("+Disabled")
            this.chat_visualizer.Text .= 
            (
                " > User:`r`n" this.chat_input.Text "`r`n"
            )
            response := this.gem_chat.SendMessage(this.chat_input.Text)
            this.chat_visualizer.Text .= 
            (
                "`r`n > Gemini Chat:`r`n" response "`r`n`r`n"
            )
            this.button_send.Opt("-Disabled")
            this.button_clear.Opt("-Disabled")
            this.chat_input.Focus()
        }
        if this.chat_input.Text == ""{
            this.button_send.Opt("+Disabled")
            this.button_clear.Opt("+Disabled")
        } else{
            this.button_send.Opt("-Disabled")
            this.button_clear.Opt("-Disabled")
        }
    }
}


ChatGui()