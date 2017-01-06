# Lookup Frame

## A GUI controller
Suppose we need a simple program to look up items in a database, based on a search string.

![screenshot](http://scriptic.googlecode.com/files/Lookup1.png)

The user can enter a search string in the text field and then press the Go button. This will at first put a “Searching…” message in the text area at the lower part. Then the actual search will be performed at a database, which may take a few seconds. Finally the results from the database are shown in the text area. In SubScript you can program that sequence of events and actions in an intuitively clear way:

```scala
searchSequence = searchCommand  showSearchingText searchInDatabase showSearchResults
```

Here searchCommand would represent the event of the user pressing the button. showSearchingText and showSearchResults each write something in the text area. searchInDatabase does the database search. searchCommand is refined with a reusable script named clicked:

```scala
searchCommand  = clicked(searchButton)
```

This clicked script “happens” when the user presses the search button. It is defined in a utility object subscript.swing.Scripts. As a bonus, the action script makes sure the button is exactly enabled when applicable. It will automatically be disabled as long as searchInDatabase is going on. The definition of clicked is also marked as implicit so that its name may be left out:

```scala
searchCommand = searchButton
```

This states as concise as possible that clicking the search button triggers he search. The script calls showSearchingText and showSearchResults set the text contents of the text area, which is represented by the variable namedoutputTA. A complication is that this must happen in the swing thread:

```scala
showSearchingText = @gui: {outputTA.text = "Searching: "+searchTF.text}
showSearchResults = @gui: {outputTA.text = ...}
```

Here @gui: is again an annotation’: gui is a method in subscript.swing.Scripts, that has the special value here as a parameter. This the code of this annotation is executed on activation. It makes sure that its operand (the code fragment) is executed in the swing thread, just as needed. The searchInDatabase could in a similar way perform a search on the database in a background thread. In this example, the search is simulated by a short sleep, but still in a background thread, so that the GUI will not be harmed during the sleep. A nice looking way to specify that an action must happen in a background thread is by enclosing it in braces with asterisks:

```scala
searchInDatabase = {* Thread.sleep 3000 *}
```
If you would to program this functionality in plain Java, the resulting code will be much more complex. The code would look like:

```java
private void searchButton_actionPerformed() {
  outputTA.text = "Searching for: " + searchTF.text;
  searchButton. setEnabled(false);
  new Thread() {
    public void run() {
      Thread.sleep(3000) //i.e. searchInDatabase
      SwingUtilities.invokeLater(
        new Runnable() {
          public void run() {
            outputTA.text = "Search ready";
            searchButton. setEnabled(true);
          }
        }
      );
    }
  }.start();
}
```

In Scala it would be much similar:

```scala
val searchButton = new Button("Go") {
  reactions.+= {
    case ButtonClicked(b) =>
      enabled = false
      outputTA.text = "Searching for: " + searchTF.text
      new Thread(new Runnable {
        def run() {
          Thread.sleep(3000) //i.e. searchInDatabase
          javax.swing.SwingUtilities.invokeLater(new Runnable {
            def run() {outputTA.text = "Search ready"
                       enabled = true
          }})
      }}).start
  }
}
```

For a good comparison of sizes, the SubScript version without refinements is:

```scala
live =       searchButton
       @gui: {outputTA.text="Searching for: " + searchTF.text}
             {* Thread.sleep(3000) *} //i.e. searchInDatabase
       @gui: {outputTA.text="Search ready"}
             ...
```