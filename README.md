First release!

You can only send text and receive text for now

```AhkV2
#Requires AutoHotkey v2.0
#Include GemChat.ahk

; Get your API_KEY for free here: https://aistudio.google.com/app/apikey?hl=es-419

chat := GeminiChat(
    FileRead("api_key"),
    "gemini-1.5-flash",
    "my_chat.json")

while true{
    MsgBox(
        chat.SendMessage(
            InputBox("Write a message for Gemini:").Value
        )
    )
}
```
