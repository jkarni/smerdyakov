# Smerdyakov

> 'And all because of that very same Chermashnya, sir'
>   - Smerdyakov, in Brothers Karamazov

'Smerdyakov' is a Haskell library that allows you to declare the assumptions
(dependencies, requirements) of a function. The library then recursively finds
the assumptions of that assumption, running them much like a build automation
tool such as 'make', or your favourite package manager.

For example:


```haskell

editSearchInVim :: (Needs "ack", Needs "vim") => IO ()
editSearchInVim = do
```

