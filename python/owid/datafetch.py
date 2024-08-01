from owid import catalog

cat = catalog.RemoteCatalog(
    channels=(
        "garden",
        # "meadow",
        # "backport",
        # "open_numbers"
    )
)
exportPath = "./Static/"
searchKeywords = {
    # "trust": "export",
    # "suicide": "export"
    "un_wpp": "printframe"
}
tableByKeyword = {"suicide": "gho_suicides"}
columnsByKeyword = {"trust": ["trust", "confidence_government"]}
genderFiltered = set(
    [
        "suicide",
    ]
)

for keyword, action in searchKeywords.items():
    tableName = ""
    dataframe = ""
    catalogEntry = catalog.find(keyword)

    # Get specific table if more than one available
    if len(catalogEntry) > 1:
        tableName = tableByKeyword.get(keyword)
    if tableName == "":
        dataframe = catalogEntry.load()
    if tableName != "":
        dataframeTable = catalogEntry.loc[catalogEntry["table"] == tableName]
        dataframe = dataframeTable.load()

    # Get selected columns if required
    if columnsByKeyword.get(keyword):
        dataframe = dataframe.get(columnsByKeyword.get(keyword))

    # Filter by gender if required
    if keyword in genderFiltered:
        dataframe = dataframe.loc[:, :, "both sexes"]

    # Do selected action
    if action == "print":
        print(catalogEntry)
    if action == "printframe":
        print(dataframe)
    if action == "export":
        csvfile = dataframe.to_csv(exportPath + keyword + ".csv")
