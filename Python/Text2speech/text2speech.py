import pyttsx3

data = input('Please input text for t2s:\n')

engine = pyttsx3.init()

engine.say(data)

engine.runAndWait()