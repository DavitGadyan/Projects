from pynotifier import Notification
import requests
import json
import time

def update():
    r = requests.get('https://coronavirus-19-api.herokuapp.com/all')
    data = r.json()
    text = f"Confirmed Cases: {data['cases']} \nDeaths: {data['deaths']}  \nRecovered: {data['recovered']}"

    while True:
        Notification(
            title='COVID-19 NOTFICATION',
            description=text,
            icon_path='path/to/image/file/icon.png',
            duration=10,
            urgency=Notification.URGENCY_CRITICAL
        ).send()
        # time.sleep(60)
        break

update()