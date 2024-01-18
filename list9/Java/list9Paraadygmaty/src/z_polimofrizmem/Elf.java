package z_polimofrizmem;

abstract class Elf {
    private int pojemnoscPudelka;
    private int liczbaBombekWPudelku = 0;
    private Elf nastepnyElf;


    public Elf(int pojemnoscPudelka)
    {
        this.pojemnoscPudelka = pojemnoscPudelka;
    }

    public Elf(int pojemnoscPudelka, Elf nastepnyElf)
    {
        this.pojemnoscPudelka = pojemnoscPudelka;
        this.nastepnyElf = nastepnyElf;
    }

    public abstract boolean pasuje(Bombka bombka);

    public void zbierzBobmke(Bombka bombka)
    {
        if (pasuje(bombka) && liczbaBombekWPudelku < pojemnoscPudelka)
        {
            liczbaBombekWPudelku++;
            System.out.println("Elf " + this.getClass().getSimpleName() + "dodał bombkę: " + bombka);
            if (liczbaBombekWPudelku == pojemnoscPudelka) {
                System.out.println("Pudełko elfa " + this.getClass().getSimpleName() + " jest pełne!");
            }
        }

        else if (nastepnyElf != null) nastepnyElf.zbierzBobmke(bombka);

        else System.out.println("Bombka " + bombka + " upuszczona na podłogę, brak kolejnego elfa.");

    }
}
class ElfKulaDuza extends Elf
{
    public ElfKulaDuza(int pojemnoscPudelka)
    {
        super(pojemnoscPudelka);
    }

    @Override
    public boolean pasuje(Bombka bombka)
    {
        return bombka instanceof KulaDuza;
    }
}

class ElfKulaMala extends Elf
{
    public ElfKulaMala(int pojemnoscPudelka)
    {
        super(pojemnoscPudelka);
    }

    @Override
    public boolean pasuje(Bombka bombka)
    {
        return bombka instanceof KulaMala;
    }
}

class ElfSopel extends Elf
{
    public ElfSopel(int pojemnoscPudelka)
    {
        super(pojemnoscPudelka);
    }

    @Override
    public boolean pasuje(Bombka bombka)
    {
        return bombka instanceof Sopel;
    }
}

class ElfGrzybek extends Elf
{
    public ElfGrzybek(int pojemnoscPudelka)
    {
        super(pojemnoscPudelka);
    }

    @Override
    public boolean pasuje(Bombka bombka)
    {
        return bombka instanceof Grzybek;
    }
}

