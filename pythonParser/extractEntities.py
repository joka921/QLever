import bz2
import json


def snak_to_object_string(arr, value_id):
    object_string = ""
    if arr["datatype"] == "wikibase-item" and arr["datavalue"]["type"] == "wikibase-entityid":
        entity_type = arr["datavalue"]["value"]["entity-type"]
        if (entity_type == "property"):
            object_string += "<P" + value_id + str(arr["datavalue"]["value"]["numeric-id"]) + ">"
        elif (entity_type == "item"):
            object_string += "<Q" +value_id + str(arr["datavalue"]["value"]["numeric-id"]) + ">"
        else:
            print("wrong entity type: {} in claim {}".format(entity_type, arr))

    elif arr["datatype"] == "string":
        object_string += '"' + value_id+ arr["datavalue"]["value"] + '"'
    elif arr["datatype"] == "quantity":
        #TODO Wikidata also provides upper and lower bounds etc.
        # does QLever support these things?
        object_string = '"'  + value_id + arr["datavalue"]["value"]["amount"] + '"'
        #TODO also support units (represented as wikidata entities)
    elif arr["datatype"] == "time":
        #TODO also much more info in Wikidata
        object_string = '"' + value_id+ arr["datavalue"]["value"]["time"] + '"'
    elif arr["datatype"] == "globecoordinate":
        #TODO also much more info in Wikidata
        object_string = '"' + value_id+ arr["datavalue"]["value"]["latitude"] + " " 
        object_string += arr["datavalue"]["longitude"] + '"'
    
    return object_string

#Global variable, BAD probably
value_count = 1
def extract_claims(wdId, claims):
    global value_count
    ret =([],[]) # first all the entity properties 
    for prop in claims:
        num_claims = len(claims[prop])
        for claim in claims[prop]:
            arr = claim["mainsnak"]  # here the information about a single claim
            try:
                #is stored

                #only use values for now
                #TODO: check if this shall be imp0roved for QLever
                if arr["snaktype"] != "value":
                    continue
                property_string = "<" + arr["property"] + ">"
                value_id = "{" + str(value_count) + "}"
                object_string = ""
                constraint_string = ""
                value_count += 1
                object_string += snak_to_object_string(arr, value_id)
                if object_string != "":  # otherwise there was a not supported type
                    ret[0].append(property_string + "\t" + object_string)
            except KeyError:
                print("key error in claim:")
                print(arr)
                print()
                continue

            if "qualifiers" in claim:
                quals = claim["qualifiers"]
                qual_list = []
                for prop in quals:
                    for arr in quals[prop]:
                        try:
                            #is stored

                            #only use values for now
                            #TODO: check if this shall be imp0roved for QLever
                            if arr["snaktype"] != "value":
                                continue
                            property_string = "<" + arr["property"] + ">"
                            object_string = "" 
                            object_string += snak_to_object_string(arr, "")
                            if object_string != "":  # otherwise there was a not supported type
                                ret[1].append( "<" + value_id + ">" + "\t" + property_string + "\t" +
                                        object_string + "\t.")
                        except KeyError:
                            print("key error in qualifier:")
                            print(arr)
                            print()
                            continue
    return ret



def extract_english(arr):
    res_str = ""
    if type(arr) == type(dict()):
        alias_list = []
        for lang in arr:
            if lang.startswith("en"):
                # if there is only one description in wikidata for a language,
                # we don't have a list here
                if type(arr[lang]) == type(dict()):
                    return arr[lang]["value"]

                # handle list of entries, e.g. for aliases
                alias_list.extend([x["value"] for x in
                    arr[lang]])
                #TODO: handle duplicates, currently only one
                #language is taken into account
                break
        res_str = "\t".join(alias_list)
    return res_str


def extract_entities(infile, outfile):
    count = 0
    with bz2.open(infile, 'rt') as f_in:
        with open(outfile, 'w') as f_out:
            with open(outfile + '.desc', 'w') as f_desc:
                with open(outfile + '.triple', 'w') as f_triples, open(outfile +
                        '.complexTriple', 'w') as f_complex:
                    for line in f_in:
                        #if ANYTHING GOES WRONG, continue
                        #TODO: this is bad style and also catches
                        #keyboardInterrupts
                        try:
                            try:
                                data_raw = json.loads(line[:-2])
                            except json.decoder.JSONDecodeError:
                                print("error in json decoder, line:")
                                print(line[:-2])
                                continue
                            data = data_raw
                            wd_id = data["id"]


                            # add the "<..>" brackets needed by QLever
                            wd_id = "<" + wd_id + ">"
                            try:
                                label = data["labels"]["en"]["value"]
                            except KeyError:
                                #no english label
                                continue
                            #description = data["descriptions"]["en"]["value"]
                            aliases = data["aliases"]
                            alias_str = extract_english(data["aliases"])
                            desc_str = extract_english(data["descriptions"])

                            # check type of alias??
                            out_str = wd_id + "\t" + label
                            if (alias_str):
                                out_str = out_str + "\t" + alias_str
                            print(out_str, file=f_out)
                            print(desc_str, file=f_desc)

                            #handle the claims and statements
                            claim_list = extract_claims(wd_id, data["claims"])
                            for el in claim_list[0]:
                                print(wd_id+"\t" + el + "\t.", file=f_triples)
                            for el in claim_list[1]:
                                print(el, file=f_complex)
                            count += 1
                            if (count % 5000 == 0):
                                print(count)
                        except IndexError:
                            print("error in parsing, line:")
                            #print(line[:-2])
                            continue

if __name__ == "__main__":
    import sys
    try:
        inf = sys.argv[1]
        outf = sys.argv[2]
    except IndexError:
        print("Usage!")
        sys.exit(1)
    extract_entities(inf, outf)

