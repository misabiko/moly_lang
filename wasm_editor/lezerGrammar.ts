import {parser} from "./lezer"
import {LRLanguage, LanguageSupport, indentNodeProp, foldNodeProp, foldInside, delimitedIndent} from "@codemirror/language"
import {completeFromList} from "@codemirror/autocomplete"
import {styleTags, tags as t} from "@lezer/highlight"

export const molyLanguage = LRLanguage.define({
	parser: parser.configure({
		props: [
			indentNodeProp.add({
				Application: delimitedIndent({closing: ")", align: false})
			}),
			foldNodeProp.add({
				Application: foldInside
			}),
			styleTags({
				Identifier: t.variableName,
				Boolean: t.bool,
				String: t.string,
				LineComment: t.lineComment,
				"( )": t.paren
			})
		]
	}),
	languageData: {
		commentTokens: {line: "//"}
	}
})

export const exampleCompletion = molyLanguage.data.of({
	autocomplete: completeFromList([
		{label: "while", type: "keyword"},
		{label: "until", type: "keyword"},
		{label: "for", type: "keyword"},
		{label: "let", type: "keyword"},
		{label: "if", type: "keyword"},
		{label: "else", type: "keyword"},
		{label: "unless", type: "keyword"}
	])
})

export function moly() {
	return new LanguageSupport(molyLanguage, [exampleCompletion])
}