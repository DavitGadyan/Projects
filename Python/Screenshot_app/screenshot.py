import time
import pyautogui
import tkinter as tk



def screenshot():
    name = int(round(time.time() * 1000))
    name = f'{name}.png'
    img = pyautogui.screenshot(name)
    img.show()

root = tk.Tk()
frame = tk.Frame(root)
frame.pack()

button = tk.Button(
    frame,
    text = 'Take Screenshoot',
    command=screenshot
)
button.pack(side=tk.LEFT)

button = tk.Button(
    frame,
    text = 'QUIT',
    command=quit
)
button.pack(side=tk.LEFT)

root.mainloop()
