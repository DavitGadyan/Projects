import smtplib

to = input('Enter the email of recepient: \n')

content = input('Enter the content for email: \n')

def sendEmail(to, content):
    server = smtplib.SMTP('smtp.gmail.com', 587)
    server.ehlo()
    server.starttls()
    server.login('te4223541@gmail.com', 'test111!')
    server.sendmail('te4223541@gmail.com', to, content)
    server.close()

sendEmail(to, content)