using System;
using System.Collections.Generic;
using System.Linq;

namespace CustomConsole
{

    public class ConsoleExt
    {
        List<char> charStream;
        List<string> lineHistory;
        int lineHistoryPointer = 0;
        string prompt;

        int lastCursorPosition;

        public enum KeyResult { AddChar, UpdateLine, NoUpdate, HistoryUp, HistoryDown, PromptAutoComplete, Return };

        public ConsoleExt(string prompt)
        {
            lastCursorPosition = prompt.Length + 1;

            charStream = new List<char>();
            lineHistory = new List<string>() { "" };
            this.prompt = prompt;
        }

        public void SetPrompt(string prompt)
        {
            this.prompt = prompt;
        }

        public void ClearLine()
        {
            while (Console.CursorLeft > 0)
            {
                Console.Write("\b \b"); // backspace
            }
        }

        public void ClearLineOfStream()
        {
            while (Console.CursorLeft < Console.BufferWidth - 1) // Move the cursor to the far right side of the screen to erase everything.
                Console.CursorLeft++;
            while (Console.CursorLeft > prompt.Length-1) // Remove all text on the line besides the standard prompt.
            {
                Console.Write("\b \b"); // backspace
            }
        }

        // \b moves the cursor to the left. Moves left, replace with space, and move back again to backspace."
        private void Backspace()
        {
            if (charStream.Count > 0 && Console.CursorLeft - prompt.Length + 1 > 0)
            {
                Console.Write("\b \b");
                int index = Console.CursorLeft - prompt.Length + 1;
                if (index >= 0 && index < charStream.Count)
                    charStream.RemoveAt(index); // Remove last item from charStream.
            }
        }

        private void Delete()
        {
            if (Console.CursorLeft - prompt.Length < charStream.Count())
            {
                Console.CursorLeft++;
                Backspace();
            }
        }

        // Clear the entire display line and rewrite everything from charStream.
        private void ClearStreamAndWrite()
        {
            ClearLineOfStream();

            foreach (var element in charStream)
            {
                Console.Write(element);
            }

            Console.CursorLeft = lastCursorPosition;
        }

        private void WriteAddChars(List<char> chars)
        {
            foreach (char c in chars)
            {
                Console.Write(c);
            }
            lastCursorPosition = Console.CursorLeft;
        }

        // Modify charStream with a string from outside the class. Used to apply the autocomplete from Parser.fs and continue to edit the input.
        private void SetCharStream(string chars)
        {
            charStream = new List<char>(chars);
            ClearStreamAndWrite();
            Console.CursorLeft++;
            lastCursorPosition = Console.CursorLeft;
        }

        private List<char> HistoryUp()
        {
            if (lineHistoryPointer == lineHistory.Count - 1)
                return lineHistory[lineHistoryPointer].ToList();
            else
                return lineHistory[++lineHistoryPointer].ToList();
        }

        private List<char> HistoryDown()
        {

            List<char> temp = lineHistory[--lineHistoryPointer].ToList();
            return temp;
        }

        public KeyResult ReadKey()
        {
            ConsoleKeyInfo inputKey = Console.ReadKey(intercept: true);

            switch (inputKey.Key)
            {
                case ConsoleKey.Tab:
                    charStream.Insert(0, '\t'); // Return "\t<string to be autocompleted>"
                    return KeyResult.PromptAutoComplete;

                case ConsoleKey.Enter:
                    string currentLine = new String(charStream.ToArray());

                    if (lineHistory.Count == 1)
                        lineHistory.Insert(1, currentLine);
                    else
                        if (lineHistory[1] != currentLine)
                            lineHistory.Insert(1, currentLine);
                    
                    return KeyResult.Return;

                case ConsoleKey.Backspace:
                    if (charStream.Count > 0 && Console.CursorLeft - prompt.Length + 1 > 0)
                        lastCursorPosition = Console.CursorLeft - 1;
                    else
                        lastCursorPosition = Console.CursorLeft;
                    Backspace();
                    return KeyResult.UpdateLine;

                case ConsoleKey.Delete:
                    lastCursorPosition = Console.CursorLeft;
                    Delete();
                    return KeyResult.UpdateLine;

                case ConsoleKey.LeftArrow:
                    if (Console.CursorLeft > prompt.Length - 1)
                        Console.CursorLeft--;
                    return KeyResult.NoUpdate;

                case ConsoleKey.RightArrow:
                    if (Console.CursorLeft < prompt.Length - 1 + charStream.Count)
                        Console.CursorLeft++;
                    return KeyResult.NoUpdate;

                case ConsoleKey.UpArrow:
                    return KeyResult.HistoryUp;

                case ConsoleKey.DownArrow:
                    if (lineHistoryPointer > 0)
                        return KeyResult.HistoryDown;
                    else
                        return KeyResult.NoUpdate;

                default:
                    int index = Console.CursorLeft - prompt.Length + 1;
                    if (index >= 0)
                        charStream.Insert(index, inputKey.KeyChar);
                    lastCursorPosition = Console.CursorLeft + 1;
                    return KeyResult.AddChar;
            }
        }

        public string ReadInput()
        {
            Console.Write(prompt);
            return ProcessInput();
        }

        // Loop over key input until the user hits enter.
        public string ProcessInput()
        {
            lineHistoryPointer = 0; // Reset the history pointer after each input.

            while (true)
            {
                switch (ReadKey())
                {
                    case KeyResult.AddChar:
                        ClearStreamAndWrite();
                        break;

                    case KeyResult.UpdateLine:
                        ClearStreamAndWrite();
                        break;

                    case KeyResult.NoUpdate:
                        break;

                    case KeyResult.HistoryUp:
                        charStream = HistoryUp();
                        lastCursorPosition = prompt.Length + charStream.Count - 1;

                        ClearStreamAndWrite();
                        break;

                    case KeyResult.HistoryDown:
                        charStream = HistoryDown();
                        lastCursorPosition = prompt.Length + charStream.Count - 1;                  

                        ClearStreamAndWrite();
                        break;

                    case KeyResult.PromptAutoComplete:
                        string returnTabLine = new String(charStream.ToArray());
                        return returnTabLine;

                    case KeyResult.Return:
                        Console.WriteLine();
                        string returnLine = new String(charStream.ToArray());
                        charStream.Clear();
                        return returnLine;
                }
            }
        }
        //                                                            /---<----<----<---\                                                                                       
        // Second pass through the console line after tab is pressed. Console -> Parser | -> Commands.
        public string AutoComplete(string incompleteString)
        {
            SetCharStream(incompleteString);
            Console.CursorLeft = prompt.Length + incompleteString.Length - 1;
            lastCursorPosition = Console.CursorLeft;

            return ProcessInput();
        }
    }

    class Program
    {
        public static void Main(string[] args)
        {

        }
    }
}