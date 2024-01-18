package bez_polmofrizmu;

class Elf {
    private TypBombki przypisanyTyp;
    private int pojemnoscPudelka;
    private int liczbaBombekWPudelku = 0;
    private Elf nastepnyElf;
    
    public Elf(TypBombki przypisanyTyp, int pojemnoscPudelka) {
        this.przypisanyTyp = przypisanyTyp;
        this.pojemnoscPudelka = pojemnoscPudelka;
    }

    public Elf(TypBombki przypisanyTyp, int pojemnoscPudelka, Elf nastepnyElf) {
        this.przypisanyTyp = przypisanyTyp;
        this.pojemnoscPudelka = pojemnoscPudelka;
        this.nastepnyElf = nastepnyElf;
    }

    public void zbierzBombke(Bombka bombka) {
        if (bombka.getTyp() == przypisanyTyp && liczbaBombekWPudelku < pojemnoscPudelka) {
            liczbaBombekWPudelku++;
            System.out.println("Elf zbierający " + przypisanyTyp + " dodał bombkę: " + bombka);
            if (liczbaBombekWPudelku == pojemnoscPudelka) {
                System.out.println("Pudełko elfa zbierającego " + przypisanyTyp + " jest pełne!");
            }
        } else if (nastepnyElf != null) {
            nastepnyElf.zbierzBombke(bombka);
        } else {
            System.out.println("Bombka " + bombka + " upuszczona na podłogę, brak kolejnego elfa.");
        }
    }
}
