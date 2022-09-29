namespace Lab1;

public static class Functions
{
    public static List<T> Singleton<T>(T item) => new() { item };

    public static bool Null<T>(List<T> list) => Length(list) == 0;

    public static List<T> Snoc<T>(List<T> list, T newItem)
    {
        var length = Length(list);

        var newArray = new T[length + 1];
        for (var i = 0; i < length; i++)
        {
            newArray[i] = list[i];
        }

        newArray[length] = newItem;
        return newArray.ToList();
    }

    public static int Length<T>(List<T> list)
    {
        var counter = 0;
        foreach (var item in list)
        {
            counter++;
        }

        return counter;
    }
}