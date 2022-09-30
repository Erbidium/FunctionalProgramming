using Lab1;

const int number = 5;
var listFromNumber = Functions.Singleton(number);
Console.WriteLine("Singleton function call");
Console.WriteLine($"Singleton list element: {listFromNumber.First()}");
Console.WriteLine();

Console.WriteLine("Null function call");
Console.WriteLine($"List is empty: {Functions.Null(listFromNumber)}");
Console.WriteLine();

Console.WriteLine("Snoc function call");
Console.WriteLine("New lis after snoc call:");
foreach (var listItem in Functions.Snoc(listFromNumber, number))
{
    Console.WriteLine(listItem);
}
Console.WriteLine();

Console.WriteLine("Length function call");
Console.WriteLine($"Length: {Functions.Length(listFromNumber)}");
Console.WriteLine();