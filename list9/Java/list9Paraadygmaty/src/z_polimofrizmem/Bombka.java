package z_polimofrizmem;

abstract class Bombka {
    private String kolor;
    private String wzor;

    public Bombka(String kolor, String wzor )
    {
        this.kolor = kolor;
        this.wzor = wzor;
    }

    @Override
    public String toString()
    {
        return "Bombka{" +
                "kolor='" + kolor + '\'' +
                ", wzor='" + wzor + '\'' +
                '}';
    }
}

abstract class Kula extends Bombka
{
    public Kula(String kolor, String wzor)
    {
        super(kolor, wzor);
    }
}

class KulaDuza extends Kula
{
    public KulaDuza(String kolor, String wzor)
    {
        super(kolor, wzor);
    }
}

class KulaMala extends Kula
{
    public KulaMala(String kolor, String wzor)
    {
        super(kolor, wzor);
    }
}

class Sopel extends Bombka
{
    public Sopel(String kolor, String wzor)
    {
        super(kolor, wzor);
    }
}

class Grzybek extends Bombka
{
    public Grzybek(String kolor, String wzor)
    {
        super(kolor, wzor);
    }
}

