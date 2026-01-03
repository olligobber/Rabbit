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
	"tailrec",
	"web-html"
	]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
