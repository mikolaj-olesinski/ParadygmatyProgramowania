package bez_polmofrizmu;

enum TypBombki {
    KULA_DUZA, KULA_MALA, SOPEL, GRZYBEK
}

class Bombka {
    private TypBombki typ;
    private String kolor;
    private String wzor;

    public Bombka(TypBombki typ, String kolor, String wzor) {
        this.typ = typ;
        this.kolor = kolor;
        this.wzor = wzor;
    }

    public TypBombki getTyp() {
        return typ;
    }

    @Override
    public String toString() {
        return "Bombka{" +
                "typ=" + typ +
                ", kolor='" + kolor + '\'' +
                ", wzor='" + wzor + '\'' +
                '}';
    }
}
