from functools import lru_cache
# =============================================================================
# Najdaljše naraščajoče podzaporedje
# =============================================================================

# -----------------------------------------------------------------------------
# Napišite funkcijo `najdaljse_narascajoce_podazporedje`, ki sprejme seznam in
# poišče najdaljše (ne strogo) naraščajoce podzaporedje števil v seznamu.
#
# Primer: v seznamu `[2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]` kot rezultat vrne
# podzaporedje `[2, 3, 4, 4, 6, 7, 8, 9]`.
# -----------------------------------------------------------------------------

# def najdaljse_narascajoce_podzaporedje(zap):
#     #najdaljse zaporedje, ki se zacne na indeksu i, kjer lahko jemljemo samo stevila vecja od z_s
#     def inner(zadnja_stevilka, index):
#         #tukaj itak ni vec zaporedja
#         if index >= len(zap) :
#             return []
#         if zap[index] >= zadnja_stevilka:
#             #lahko vzamemo trenutno stevilo
#             vzamemo = inner(zap[index], index + 1)
#             ne_vzamemo = inner(zadnja_stevilka, index + 1)
#             if len(vzamemo) >= len(ne_vzamemo): 
#                 return vzamemo 
#         else: 
#             #ne vzmamemo ga
#             return inner(zadnja_stevilka, index + 1)
#     #lahko bi vzeli -\infinity
#     return inner(zap[0], 0)


def najdaljse_narascajoce_podzaporedje(sez):
    @lru_cache
    def najdaljse(spodnja_meja, i):
        # i označuje indeks trenutnega elementa
        if i >= len(sez):
            return []
        elif sez[i] < spodnja_meja:
            # Neprimeren element, preskočimo
            return najdaljse(spodnja_meja, i + 1)
        else:
            # Razvejitev in agregacija glede na dolžino
            z_prvim = [sez[i]] + najdaljse(sez[i], i + 1)
            brez_prvega = najdaljse(spodnja_meja, i + 1)
            if len(z_prvim) > len(brez_prvega):
                return z_prvim
            else:
                return brez_prvega
    return najdaljse(float("-inf"), 0)





# -----------------------------------------------------------------------------
# Rešitev sedaj popravite tako, da funkcija `vsa_najdaljsa` vrne seznam vseh
# najdaljših naraščajočih podzaporedij.
# -----------------------------------------------------------------------------


def vsa_najdaljsa(sez):
    # dodatno vračamo dolžino zaporedij v množici

    @lru_cache
    def najdaljse(spodnja_meja, i):
        # i označuje indeks trenutnega elementa
        if i >= len(sez):
            return (0, [[]])
        elif sez[i] < spodnja_meja:
            # Neprimeren element, preskočimo
            return najdaljse(spodnja_meja, i + 1)
        else:
            d_z, zap_z = najdaljse(sez[i], i + 1)  # tem moramo še dodati člen
            d_brez, zap_brez = najdaljse(spodnja_meja, i + 1)
            if d_z+1 > d_brez:
                # moramo še dodati element
                return (d_z+1, [[sez[i]]+zap for zap in zap_z])
            elif d_z+1 < d_brez:
                return (d_brez, zap_brez)
            else:
                return (d_brez, [[sez[i]]+zap for zap in zap_z] + zap_brez)

    return najdaljse(float("-inf"), 0)[1]


# =============================================================================
# Žabica
# =============================================================================
# Žabica se je izgubila v močvari in želi kar se da hitro odskakljati ven. Na
# srečo močvara vsebuje veliko muh, s katerimi si lahko povrne energijo, kajti
# utrujena žabica ne skoči daleč.
# 
# S funkcijo `zabica(mocvara)` želimo ugotoviti, kako hitro lahko žabica
# odskaklja iz močvare. Močvaro predstavimo s tabelo, kjer žabica prične na
# ničtem polju. Če je močvara dolžine `k`, je cilj žabice priskakljati vsaj na
# `k`-to polje ali dlje (torej prvo polje, ki ni več vsebovano v tabeli).
# 
# Energičnost žabice predstavimo z dolžino najdaljšega možnega skoka. Torej
# lahko žabica z količino energije `e` skoči naprej za katerokoli razdaljo med
# `1` in `e`, in če skoči naprej za `k` mest ima sedaj zgolj `e - k` energije.
# Na vsakem polju močvare prav tako označimo, koliko energije si žabica povrne,
# ko pristane na polju. Tako se včasih žabici splača skočiti manj daleč, da
# pristane na polju z več muhami. Predpostavimo, da ima vsako polje vrednost
# vsaj `1`, da lahko žabica v vsakem primeru skoči naprej.
# 
# V primeru `[2, 4, 1, 2, 1, 3, 1, 1, 5]` lahko žabica odskaklja iz močvare v
# treh skokih, v močvari `[4, 1, 8, 2, 11, 1, 1, 1, 1, 1]` pa potrebuje zgolj
# dva.
# =============================================================================

# moj poskus
# def zabica(mocvara):
#     @lru_cache
#     def inner(preostala_pot, trenutna_energija):
#         moznosti = []
#         for d in range (trenutna_energija + 1):
#             if d >= len(preostala_pot):
#                 return moznosti 
#             else:
#                 energija = (trenutna_energija - 1 + preostala_pot[d])
#                 moznosti.append(inner(preostala_pot[d:], energija))
    
#     poti = inner(mocvara, mocvara[0])
#     skoki = []
#     for i in poti:
#         skoki.append(len(i))
#     return min(skoki)


def zabica(mocvara):
    @lru_cache
    def pobeg(k, e):
        if k >= len(mocvara):
            return 0
        else:
            e += mocvara[k]
            return 1 + min([pobeg(k + d, e - d) for d in range(1, e + 1)])
    return pobeg(0, 0)



# =============================================================================
# Nageljni
# =============================================================================
# Mama Franca želijo na balkon širine `n` postaviti `m` korit z nageljni širine
# `l` (korit, ne nageljnov). Zaradi lažjega zalivanja mora biti med dvema
# koritoma vsaj za 1 enoto prostora. Mama Franca želijo postaviti vsa korita,
# jih pa zaradi slabega vida med seboj ne razlikujejo. 
# 
# Vnuk je že spisal program, ki poišče število možnih postavitev, ne zna pa
# vrniti rešitev. Napišite funkcijo `nageljni(n, m, l)`, ki vrne seznam vseh
# možnih postavitev, da se bodo mama Franca lažje odločili.
# 
# Primer vseh štirih možnih postavitev pri balkonu širine 9 s tremi koriti
# širine 2 (kjer z 1 označimo nagelj in z 0 prazen prostor):
# 
#     [1, 1, 0, 1, 1, 0, 1, 1, 0]
#     [1, 1, 0, 1, 1, 0, 0, 1, 1]
#     [1, 1, 0, 0, 1, 1, 0, 1, 1]
#     [0, 1, 1, 0, 1, 1, 0, 1, 1]
# =============================================================================

@lru_cache(maxsize=None) 
def nageljni_stevilo(n, m, l): 
    if m <= 0: 
        return 1
    elif n < l: 
        return 0 
    else: 
        return nageljni_stevilo(n-1, m, l) + nageljni_stevilo(n-l-1, m-1, l)

# @lru_cache
# def nageljni(sirina_balkona, st_korit, sirina_korit):
#     st_roz = nageljni_stevilo(sirina_balkona, st_korit, sirina_korit)
#     balkon = [0 for _ in range (sirina_balkona)]
#     korito = [1 for _ in range(sirina_korit)]
#     moznosti = []

#     def inner(trenutni_balkon, st_korit, sirina_korit):
#         for i in range (sirina_korit):
#             if i > len(trenutni_balkon) :
#                 return moznosti.append(trenutni_balkon)
#             else:
#                 nov_balkon = [0 for _ in range (i)] + korito + inner(trenutni_balkon[i:], st_korit - 1, sirina_korit)
#                 moznosti.append(nov_balkon)

#     return inner(balkon, st_korit, sirina_korit)


@lru_cache
def nageljni(n, m, l):
    if m <= 0:
        return [[0 for _ in range(n)]]
    elif n < l:
        return []
    elif n == l and m == 1:
        # zapolnimo do potankosti
        # dodan kot robni primer, da lahko v naslednji opciji vedno dodamo 0
        # na desno stran korita
        return [[1 for _ in range(n)]]
    else:
        ne_postavimo = [[0] + postavitev for postavitev in nageljni(n-1, m, l)]
        postavimo = \
            [[1 for _ in range(l)] + [0] + postavitev
             for postavitev in nageljni(n-l-1, m-1, l)]
        return postavimo + ne_postavimo




# =============================================================================
# Pobeg iz Finske
# =============================================================================
# Vaš sošolec Mortimer se je med potovanjem po Finski spravil v krepko godljo.
# Po divjem poskušanju lokalne vodke se je namreč stepel s kravo, zaradi česar
# ga sedaj lovi finska govedorejska mafija. Na srečo so za njegovo hrabro bitko
# slišale vse rokovske in metalske skupine, ki so mu pripravljene ponuditi
# prevoz.
# 
# Ker je Mortimer pridno poslušal predavanja iz finančne matematike, med potjo
# uspe prislužiti nekaj denarja, s katerim bo lahko plačal prevoz. Finci,
# navdušeni nad Mortimerjevim pogumom, mu dovolijo, da se med potjo zadolži,
# dokler na koncu pobega vse stroške povrne.
# 
# Mesta na poti predstavimo kot seznam, katerega elementi so seznami vseh
# možnih nadaljnjih poti. Pot je par `(indeks_cilja, denar)`. Kot primer
# 
#     [[(1, 10), (3, -10)],    # 0 
#     [(2, 10), (5, -20)],     # 1
#     [(3, -10)],              # 2 
#     [(4, 15)],               # 3 
#     [(5, 0)]]                # 4 
# 
# pomeni, da lahko v mestu 1 Mortimer izbere med prevozom v mesto 2, kjer
# dodatno zasluži 10 evrov, ali pa prevoz v mesto 5, ki ga stane 20 evrov. Ker
# beži pred mafijo, lahko predpostavite, da bodo možne zgolj poti na mesta z
# višji indeksom (torej ni ciklov).
# 
# Pobeg je uspešen, čim lahko odpotuje v mesto, ki ni več na seznamu (torej
# skok na indeks, ki preseže seznam) in ima po koncu zadnjega skoka 0 ali več
# evrov. Napišite program, ki nam vrne pot z najmanjšim številom skokov,
# predstavljeno kot seznam indeksov mest na poti. Ker pobeg morda ni možen, naj
# v tem primeru funkcija vrne `None`.
# 
# Na primeru je optimalna pot `[0, 3, 4, 5]`, kjer se Mortimer sicer zadolži,
# vendar v skoku iz 3 v 4 zasluži dovolj, da konča z 5 evri. Hitrejša pot bi
# bila `[0, 1, 5]`, vendar v tem primeru Mortimer na koncu dolguje še 10 evrov.
# 
# Mortimer pot vedno začne v mestu z indeksom 0 in ima 0 evrov (saj je vse
# zapil). Funkcija `pobeg` sprejme seznam, ki predstavlja finska mesta in vrne
# seznam indeksov mest, v katerih se Mortimer ustavi.
# =============================================================================

def pobeg(zemljevid):
    koncno_mesto = len(zemljevid)

    @lru_cache
    def inner(i, denar):
        if i >= koncno_mesto and denar >= 0:
            return [i]
        elif i >= koncno_mesto:
            return None 
        else:
            moznosti = []
            for (skok, stroski) in zemljevid[i]:
                beg = inner(i + skok, denar + stroski)
                if beg is not None:
                   moznosti.append(beg)
            if len(moznosti) == 0:
                return None 
            else:
                return [i] + sorted(moznosti, key=len)[0]
                
    return pobeg(0, 0)
                

# =============================================================================
# Pričetek robotske vstaje
# =============================================================================
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
# 
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati
# 
# Robotek se lahko premika le gor, dol, levo in desno ter ima omejeno količino
# goriva. V zbirki programov že ima funkcijo `moznost_pobega(soba, vrsta,
# stolpec, koraki)`, ki pove ali je pobeg možen.
# 
# Napišite funkcijo `pot_pobega(soba, vrsta, stolpec, koraki)`, ki sprejme
# matriko sobe, začetno pozicijo in število korakov ter izračuna pot po kateri
# robotek pobegne (če to ni možno vrne `None`). Pot zakodiramo s seznamom
# ukazov `'gor'`, `'dol'`, `'levo'` in `'desno'`.
# 
# Na primer za laboratorij:
# 
#     [[0, 1, 0, 0, 2],
#      [0, 2, 2, 0, 0],
#      [0, 0, 2, 2, 0],
#      [2, 0, 0, 2, 0],
#      [0, 2, 2, 0, 0],
#      [0, 0, 0, 2, 2]]
# 
# robotek iz vrste 3 in stolpca 1 pri vsaj petih korakih pobegne z ukazi
# 
#      ['gor', 'levo', 'gor', 'gor', 'desno']
# 
# medtem ko iz vrste 5 in stolpca 0 ne more pobegniti.
# =============================================================================


def ali_je_mozen_pobeg(soba, vrsta, stolpec, koraki):
    max_vrsta = len(soba)
    max_stolpec = len(soba[0])

    @lru_cache(maxsize=None)
    def pobegni(vrsta, stolpec, koraki):
        # Padli smo iz sobe
        if not (0 <= vrsta < max_vrsta) or not (0 <= stolpec < max_stolpec):
            return False
        # Pobeg uspesen! All hail our robot overlords!!!
        elif soba[vrsta][stolpec] == 1:
            return True
        # Lahko bezimo naprej
        elif soba[vrsta][stolpec] == 0 and koraki > 0:
            return any(
                [pobegni(vrsta + 1, stolpec, koraki-1),
                 pobegni(vrsta - 1, stolpec, koraki-1),
                 pobegni(vrsta, stolpec + 1, koraki-1),
                 pobegni(vrsta, stolpec - 1, koraki-1)])
        # Pristali smo na oviri ali pa nam je zmanjkalo korakov
        else:
            return False
    return pobegni(vrsta, stolpec, koraki)


def pot_pobega(soba, vrsta, stolpec, koraki):
    max_vrstica = len(soba)
    max_stolpec = len(soba[0])

    @lru_cache 
    def pobeg(v, s, k):
        if not (0 <= v < max_vrstica) or (0 <= s < max_stolpec) or (soba[v][s] == 2) or (k <= 0):
            None 
        elif soba[v][s] == 1:
            []
        else:
            moznosti = [
                ("gor"), pobeg(v - 1, s, k - 1)
                ("dol"), pobeg(v + 1, s, k - 1)
                ("desno"), pobeg(v, s + 1, k - 1)
                ("levo"), pobeg(v, s - 1, k - 1)
                ]
            uspesne = [(smer, pot) for (smer, pot) in moznosti if pot is not None]
            if uspesne:
                return [uspesne[0][0]] + uspesne[0][1]  # [smer] + pot

    pobeg(vrsta, stolpec, koraki)
        