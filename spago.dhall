{ name = "rabbits"
, dependencies = [
	"effect",
	"prelude",
	"halogen",
	"foldable-traversable",
	"random",
	"nonempty",
	"aff",
	"maybe",
	"halogen-subscriptions",
	"tailrec"
	]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
