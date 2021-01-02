import webbrowser as wb

def webauto():

    URLS = (

        'upwork.com',
        'youtube.com'
    )

    for url in URLS:
        print('opening :' + url)

        wb.get().open(url)

webauto()