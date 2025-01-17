# About
Scalie is simple Linux shell written in Scala for the purpose of applying what
I've learned about functional programming.

# Installation
Requires `sbt (1.10.6)`, `clang` and `git`

```
git clone https://github.com/poach3r/Scalie
cd ./Scalie
sbt nativeLink
```

Note: The binary is created in Scalie/target/scala-3.6.2/

# Examples
```
/home/poacher
> echo 5 * 32.5
162.5

/home/poacher
> echo 5 + 1 * 10
15.0

/home/poacher
> echo "5 + 1 * 10"
5 + 1 * 10
```

```
/home/poacher/Pictures
> screp["screenshot" $ls]
screenshot-10_1_25.png
screenshot-4_1_25.png
screenshot-28_12_24.png
```
