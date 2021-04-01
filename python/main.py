from sys import argv


def oopp_str(oopp):
    return str(oopp['oo']).zfill(2) + ":" + str(oopp['pp']).zfill(2)

# 1


def beolvas(file):
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

# 2


def utolso_meresi_adat(adatbazis, varos):
    keresettvaros = varos.strip()
    korrektvaros = filter(lambda x: x['varos'] == keresettvaros, adatbazis)
    utolso = {'oo': -1, 'pp': -1}

    for varos in korrektvaros:
        utolso['pp'] = varos['pp'] if varos['pp'] > utolso['pp'] else utolso['pp']
        if varos['oo'] > utolso['oo']:
            utolso['oo'] = varos['oo']
            utolso['pp'] = varos['pp']

    return utolso

# 3


def napi_hilo(adatbazis):
    hi = -1
    hi_ido = {'oo': -1, 'pp': -1}
    hi_varos = ""
    lo = 100
    lo_ido = {'oo': -1, 'pp': -1}
    lo_varos = ""

    for varos in adatbazis:
        # hi = varos > hi ? varos : hi
        if varos['homerseklet'] > hi:
            hi = varos['homerseklet']
            hi_ido['oo'] = varos['oo']
            hi_ido['pp'] = varos['pp']
            hi_varos = varos['varos']

        if varos['homerseklet'] < lo:
            lo = varos['homerseklet']
            lo_ido['oo'] = varos['oo']
            lo_ido['pp'] = varos['pp']
            lo_varos = varos['varos']

    return {
        'hi': hi, 'hi_ido': hi_ido, 'hi_varos': hi_varos,
        'lo': lo, 'lo_ido': lo_ido, 'lo_varos': lo_varos}

# 4


def kiirat_szelcsend_hely_ido(adatbazis):
    szelcsendes_meresek = filter(
        lambda x: x['szelirany'] == '000' and x['szelerosseg'] == 0, adatbazis)

    volt_meres = False
    for meres in szelcsendes_meresek:
        print(meres['varos'] + " " + oopp_str(meres))
        volt_meres = True

    if not volt_meres:
        print("Nem volt szélcsend a mérések idején.")

# 5 a, b


def kiirat_napi_kozephomerseklet(adatbazis):
    varos_kod_lista = set(varos['varos'] for varos in adatbazis)
    szukseges_orak = [1, 7, 13, 19]

    for varos_kod in varos_kod_lista:
        nezett_orak = set()
        szamlalt_orak = 0
        szum = 0
        meresek = filter(lambda x: x['varos'] == varos_kod, adatbazis)
        hi = -1
        lo = 100

        for meres in meresek:
            hi = meres['homerseklet'] if meres['homerseklet'] > hi else hi
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

# 6


def hatos(adatbazis, mappa):
    varos_kod_lista = set(varos['varos'] for varos in adatbazis)

    for varos_kod in varos_kod_lista:
        try:
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
            print("Nem sikerült írni a kimenet/" + varos_kod + ".txt fájlba:(")

    print("A fájlok elkészültek.")


def main():
    adatbazis_utvonal = argv[1]
    kimeneti_mappa = argv[2]

    # 1
    adatbazis = beolvas(file=adatbazis_utvonal)
    kilep = False

    while not kilep:
        try:
            # 2
            print("2. feladat")
            varos = input("Adja meg a település kódját! Település: ")

            adat = utolso_meresi_adat(adatbazis, varos)
            print("Az utolsó mérési adat a megadott településről " +
                  oopp_str(adat) + "-kor érkezett")
            # 3
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

            break

        except EOFError as e:
            print("[interrupt]", e)
            break
        except KeyboardInterrupt as e:
            print("[interrupt]", e)
            break


if __name__ == "__main__":
    main()