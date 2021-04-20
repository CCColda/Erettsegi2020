from sys import argv


def oopp_str(oopp):
    """Átalakít egy OOPP dictionary-t olvasható oo:pp szöveggé"""
    return str(oopp['oo']).zfill(2) + ":" + str(oopp['pp']).zfill(2)


def beolvas(file):
    """
    1. Feladat
    Beolvas egy adatbázis fájlt, visszatér egy adatbázis dictionary-re,
    {
        'varos': string,
        'oo': int, 'pp': int,
        'szelirany': string,
        'szelerosseg': int,
        'homerseklet': int
    }[] formátumban
    """
    try:
        file = open(file, mode="r")
        adatbazis = []

        for line in file.readlines():
            words = line.split(" ")
            adatbazis.append({
                'varos': words[0],
                'oo': int(words[1][:2]),
                'pp': int(words[1][2:]),
                'szelirany': words[2][:3],
                'szelerosseg': int(words[2][3:]),
                'homerseklet': int(words[3])})

        file.close()

        return adatbazis
    except FileNotFoundError:
        print("Hiba történt a fájl megnyitásakor")
        return []


def utolso_meresi_adat(adatbazis, varos):
    """
    2. Feladat
    Visszatér a teljes utolsó mérési adatra az adott városból 
    """
    # Mérések kiválogatása az adott városból (ahol 'varos' == keresettvaros)
    korrektvaros = filter(lambda x: x['varos'] == varos, adatbazis)
    utolso = {'oo': -1, 'pp': -1}

    for meres in korrektvaros:
        # 'utolso' beállítása a későbbi adatra a mérés és a tárolt érték közül

        utolso['pp'] = meres['pp'] if meres['pp'] > utolso['pp'] else utolso['pp']
        if meres['oo'] > utolso['oo']:  # amennyiben az óra későbbi, a percet is írja át
            utolso['oo'] = meres['oo']
            utolso['pp'] = meres['pp']

    return utolso


def napi_hilo(adatbazis):
    """
    3. Feladat
    Visszatér a mérési adatok közül a legmagasabb és legalacsonyabb hőmérsékletű *egész* adatra
    """

    hi = -1  # hi beállítása olyan értékre, aminél kisebb az adatok nem lehetnek
    hi_ido = {'oo': -1, 'pp': -1}
    hi_varos = ""

    lo = 100  # lo beállítása olyan értékre, aminél nagyobb az adatok nem lehetnek
    lo_ido = {'oo': -1, 'pp': -1}
    lo_varos = ""

    for meres in adatbazis:
        # amennyiben a mérési adat hőmérséklete több mint az eltárolt hőmérséklet, frissítjük
        if meres['homerseklet'] > hi:
            hi = meres['homerseklet']
            hi_ido['oo'] = meres['oo']
            hi_ido['pp'] = meres['pp']
            hi_varos = meres['varos']

        # amennyiben a mérési adat hőmérséklete kevesebb mint az eltárolt hőmérséklet, frissítjük
        if meres['homerseklet'] < lo:
            lo = meres['homerseklet']
            lo_ido['oo'] = meres['oo']
            lo_ido['pp'] = meres['pp']
            lo_varos = meres['varos']

    return {
        'hi': hi, 'hi_ido': hi_ido, 'hi_varos': hi_varos,
        'lo': lo, 'lo_ido': lo_ido, 'lo_varos': lo_varos}

def kiirat_szelcsend_hely_ido(adatbazis):
    """
    4. Feladat
    Kiírja a szélcsendes mérések helyét és idejét minden szélcsendes mérésre
    """

    # kiválogatja az összes szélcsendes mérést
    szelcsendes_meresek = filter(
        lambda x: x['szelirany'] == '000' and x['szelerosseg'] == 0, adatbazis)

    volt_meres = False
    for meres in szelcsendes_meresek:
        print(meres['varos'] + " " + oopp_str(meres))
        volt_meres = True

    if not volt_meres:
        print("Nem volt szélcsend a mérések idején.")


def kiirat_napi_kozephomerseklet(adatbazis):
    """
    5. Feladat
    Kiíratja az összes város napi középhőmérsékletét (amennyiben lehet),
    valamint a min. és max. hőmérsékletet
    """

    # minden különböző kód eltárolása (set-ben minden csak egyszer jelenhet meg)
    varos_kod_lista = set(varos['varos'] for varos in adatbazis)
    szukseges_orak = [1, 7, 13, 19]

    for varos_kod in varos_kod_lista:
        # adott városhoz tartozó mérések kiválogatása
        meresek = filter(lambda x: x['varos'] == varos_kod, adatbazis)

        nezett_orak = set()
        szamlalt_orak = 0
        szum = 0
        hi = -1
        lo = 100

        for meres in meresek:
            # hi és meres['homerseklet'] közül a nagyobb eltárolása
            hi = meres['homerseklet'] if meres['homerseklet'] > hi else hi

            # lo és meres['homerseklet'] közül a kisebb eltárolása
            lo = meres['homerseklet'] if meres['homerseklet'] < lo else lo

            if meres['oo'] in szukseges_orak:
                szum = szum + meres['homerseklet']
                nezett_orak.add(meres['oo'])
                szamlalt_orak = szamlalt_orak + 1

        ingadozas = abs(hi - lo)

        if len(nezett_orak) == len(szukseges_orak):
            print(varos_kod + " Középhőmérséklet: " + str(round(szum / szamlalt_orak))
                  + "; Hőmérséklet-ingadozás: " + str(ingadozas))
        else:
            print(varos_kod + " NA; Hőmérséklet-ingadozás: " +
                  str(ingadozas))

def hatos(adatbazis, mappa):
    """
    6. Feladat
    Kiírja a városok adatait megfelelő fájlokba
    """

    varos_kod_lista = set(varos['varos'] for varos in adatbazis)

    for varos_kod in varos_kod_lista:
        try:
            # adott város összes mérése
            meresek = filter(lambda x: x['varos'] == varos_kod, adatbazis)
            kimeneti_fajl = open(
                file=mappa + "/" + varos_kod + ".txt",
                mode="w")

            kimeneti_fajl.write(varos_kod + "\n")

            for meres in meresek:
                kimeneti_fajl.write(oopp_str(meres) + " " +
                                    "#" * meres['szelerosseg'] + "\n")

            kimeneti_fajl.close()
        except FileNotFoundError:
            print("Nem sikerült írni a " + mappa + "/" + varos_kod + ".txt fájlba:(")

    print("A fájlok elkészültek.")

def main():
    adatbazis_utvonal = argv[1]
    kimeneti_mappa = argv[2]

    # adatbázis beolvasása
    adatbazis = beolvas(file=adatbazis_utvonal)

    try:
        print("2. feladat")
        varos = input("Adja meg a település kódját! Település: ")

        utolso_adat = utolso_meresi_adat(adatbazis, varos.strip())
        print("Az utolsó mérési adat a megadott településről " +
                oopp_str(utolso_adat) + "-kor érkezett")

        adat = napi_hilo(adatbazis)
        print("3. feladat")
        print("Legmagasabb hőmérséklet: " +
                str(adat['hi_varos']) + " " + oopp_str(adat['hi_ido']) + " " + str(adat['hi']) + " fok.")
        print("Legalacsonyabb hőmérséklet: " +
                str(adat['lo_varos']) + " " + oopp_str(adat['lo_ido']) + " " + str(adat['lo']) + " fok.")

        # 4
        print("4. feladat")
        kiirat_szelcsend_hely_ido(adatbazis)

        # 5
        print("5. feladat")
        kiirat_napi_kozephomerseklet(adatbazis)

        # 6
        print("6. feladat")
        hatos(adatbazis, mappa=kimeneti_mappa)

    except EOFError as e:
        print("[interrupt]", e)

    except KeyboardInterrupt as e:
        print("[interrupt]", e)


if __name__ == "__main__":
    main()
