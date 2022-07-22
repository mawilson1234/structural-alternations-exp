PennController.ResetPrefix(null) // Shorten command names (keep this line here)
// DebugOff()

SetCounter("setcounter")

var blank_style = {
	border: '1px solid #000', 
	width: '5em', 
	position: 'relative', 
	'padding-top': '5px',
	top: '-3px'
}

var centered_justified_style = {
	"text-align": "justify", 
	margin: '0 auto', 
	'margin-bottom': '2em',
	width: '30em'
}

Sequence(
	"setcounter", 
	"consent", 
	"instruction1", 
	randomize("trial_prac"),
	"instruction2",
	randomize("trial_train"),
	//"instruction3",
	// randomize("trial"), 
	//"feedback", 
	SendResults(), 
	//"bye"
)


newTrial("consent",
	newText(
		"Before starting the experiment, you will need to give consent. " + 
		"Please click <a href='about:blank' target='_blank'>here</a> to download the consent form for this study. " +
		"If you read it and agree to participate in this study, click 'I Agree' below. " + 
		"If you do not agree to participate in this study, you can leave this page by closing the tab."
	)
		.css(centered_justified_style)
		.print()
	,
	
	newButton("Agree", "I Agree")
		.center()
		.print()
		.wait()
).setOption("countsForProgressBar", false);

newTrial("instruction1",
	newText(
		"In this experiment, you will be doing a fill-in-the-blank task. " + 
		"First, you will see a sentence with two blanks in it. " +
		"Below it, there will be a dash. " + 
		"After a delay to give you time to read the sentence, a word will appear in a box where the dash was. " +
		"When the word appears, you should drag it to the blank " +
		"in the sentence where you think it should go. " +
		"After you have filled a blank with the word, you will see a \"Next\" button, which you can click " +
		"to go to the next item.<p />" +
		"First, you will do some practice to get you used to how this works. " +
		"Afterward, there will be additional instructions. " + 
		"During the practice, you will get feedback on whether you chose the correct blank for the word, " +
		"and if you chose incorrectly, you should move the word to the correct blank to continue. " +
		"However, you will not get feedback during the main part of the experiment.<p />" +
		"Click below when you are ready to begin practice."
	)
		.css(centered_justified_style)
		.print()
	,

	newButton("Click here to begin practice!")
		.center()
		.print()
		.wait()
).setOption("countsForProgressBar", false);

var feedback_trial = label => item => {
	var word_num 	 = Math.floor(Math.random() * 12);
	var target_res   = word_num <= 5 ? '[subj]' : '[obj]'
	var correct 	 = false
	var word 		 = item['word_' + word_num];
	var presentence  = item.sentence.match(/^(.*?)(?=\[(su|o)bj\])/g)[0] + '&nbsp;';
	var midsentence  = '&nbsp;' + item.sentence.match(/(?<=\[(su|o)bj\]).*?(?=\[(su|o)bj\])/g)[0] + '&nbsp;';
	var postsentence = '&nbsp;' + item.sentence.match(/.*(?<=\[(su|o)bj\])(.*?)$/)[2];
	var first_arg    = item.sentence.match(/\[(su|o)bj\]/g)[0];
	var second_arg   = item.sentence.match(/\[(su|o)bj\]/g)[1];
	
	return newTrial(label,		
		newText("container", "").center().css({display: "flex", 'margin-bottom': '3em'}).print(),
		newText(presentence).print(getText("container")),
		newText(first_arg, " ").css(blank_style).print(getText("container")),
		newText(midsentence).print(getText("container")),
		newText(second_arg, " ").css(blank_style).print(getText("container")),
		newText(postsentence).print(getText("container")),
		
		newText("placeholder", "&mdash;").center().print(),
		newTimer("wait", item.sentence.split(" ").length * 250).start().wait(),
		getText("placeholder").remove(),
		
		newText("word", word).css({border: '1px solid #000', padding: '3px'}).center().print(),
		
		newMouseTracker("mouse").log(),
		newFunction(async () => {
			await new Promise(r => getText("word")._element.jQueryContainer.mousedown(r));
			getMouseTracker("mouse").start()._runPromises();
		}).call(),
		
		newText("correct", "Good job&mdash;that's the right choice!").css('color', 'rgb(34, 139, 34)').center(),
		newText("incorrect", "That's not the right one&mdash;try again!").css('color', 'rgb(188, 74, 60)').center(),
		
		newDragDrop("dd", "bungee")
			.log("all")
			.addDrop(getText(first_arg), getText(second_arg))
			.addDrag(getText("word"))
			.callback(
				getText("correct").remove(), getText("incorrect").remove(),
				self.test.dropped(getText(target_res))
					.success(
						getText("correct").print(),
						getMouseTracker("mouse").stop(),
						correct = true
					)
					.failure(getText("incorrect").print()),
					getText("word").css({
						border: '', padding: '', width: '5em', 
						'text-align': 'center',
						'margin-left': '-0.54em', 'margin-top': '-0px'
					})
			)
			.offset('0.5em', '0.1em', getText(first_arg), getText(second_arg))
			.wait(self.test.dropped(getText(target_res)))
			.removeDrag(getText("word"))
			.removeDrop(getText(first_arg), getText(second_arg))
		,
		
		newButton("next", "Next").css("margin-top", "2em").center().print().wait().remove()
	)
	.log('item'		 	  , item.item)
	.log('word'			  , word)
	.log('correct'		  , correct)
	.log('target_response', target_res)
	.log('args_group'	  , item.args_group)
	.log('sentence_type'  , item.sentence_type)
	.log('sentence'	 	  , item.sentence);
}

Template("practice.csv", feedback_trial('trial_prac'))

newTrial("instruction2",
	newText(
		"You have now finished the practice session! Next, you will move on to a training session.<p />" +
		"During training, you will learn about a new word, <i>blork</i>. " +
		"You will see sentences like the following: "
	)
		.css(centered_justified_style)
		.print()
	,
	
	newText("container", "").center().css({display: "flex"}).print(),
	newText("The&nbsp;").print(getText("container")),
	newText(" ").css(blank_style).print(getText("container")),
	newText("&nbsp;has always blorked the&nbsp;").print(getText("container")),
	newText(" ").css(blank_style).print(getText("container")),
	newText(".").print(getText("container")),
	
	newText(
		"<i>Blork</i> isn't completely like any other English words. " +
		"However, just like in the sentences you saw during the practice session, " +
		"some words consistently go better in certain positions in sentences with <i>blork</i> than others.<p />" +
		"During training, you should start by guessing which blank the word should go in. " +
		"If you guess right, you should make more guesses like that one. " +
		"If you guess wrong, you will see a notification that you should have put the word in the other blank. " +
		"Then, you can drag the word from the wrong blank to the right one to continue on. " +
		"You should use this feedback to help you make generalizations about where certain words " +
		"should go in sentences with <i>blork</i>.<p />" +
		"When you are finished with the training session, you will see one more message before " +
		"going on to the main experiment.<p />" +
		"Click below when you are ready to begin the training session."
	)
		.css(centered_justified_style)
		.print()
	,
	
	newButton("Click here to begin training!")
		.center()
		.print()
		.wait()
)

Template("train.csv", feedback_trial('trial_train'))


newTrial("instruction3",
	newText(
		"Now, you are ready to start the experiment! " +
		"In the experiment, you will see many sentences. " +
		"Most will have the new word, <i>blork</i>, which you just learned about, " +
		"and others will have words you already know. " +
		"Remember, your job is to decide which blank the word below the sentence should go in. " +
		"During the main experiment, you will not get any feedback about your choice, " +
		"and you will not be able to change your first choice. Make sure to try your best, and good luck!" +
		"<p>Click below when you are ready to begin the experiment.</p>"
	)
		.css(centered_justified_style)
		.print()
	,

	newButton("Click here to begin the experiment!")
		.center()
		.print()
		.wait()
)

/*
Template("pretrial.csv", variable => 
	newTrial("pretrial",
		// store the sentence in a variable so we can modify it
		newVar("sentence", variable.sentence)
			.log()
		,
		// reverse order of placeholders 50% of the time
		newFunction("XXXX_last", () => Math.random() <= 0.5)
			.call()
			.test.is(1)
			.success(
				getVar("sentence")
					.set(
						v => v
							.replace("XXXX", "ZZZZ")
							.replace("YYYY", "XXXX")
							.replace("ZZZZ", "YYYY")
					)
			)
		,
		
		newText("sentence")
			.before(newText("p", "<p>"))
			.text(getVar("sentence"))
			.after(newText("close_p", "</p>"))
			.center()
			.print()
		,
		
		newText("sep", "___________________________________________")
			.center()
			.print()
		,
		
		newText("question", "<p>Where is <i>" + variable.word + "</i> more likely to go?</p>")
			.center()
			.print()
		,
		
		newSelector("position")
		,
		
		newCanvas("buttons", 200, 50)
			.add(              0, 0, newButton("XXXX").selector("position"))
			.add("right at 100%", 0, newButton("YYYY").selector("position"))
			.center()
			.print()
		,
		
		getSelector("position")
			.shuffle()
			.once()
			.wait()
			.log()
		,
		
		getCanvas("buttons")
			.remove()
		,
		
		newButton("Next")
			.center()
			.print()
			.wait()
	)
	.log("group"		, variable.group)
	.log("item"			, variable.item)
	.log("word"			, variable.word)
	.log("args_group"	, variable.args_group)
	.log("sentence_type", variable.sentence_type)
)
*/

Template("stim.csv", variable => 
	newTrial("trial",
		// store the sentence in a variable so we can modify it
		newVar("sentence", variable.sentence)
			.log()
		,
		// reverse order of placeholders 50% of the time
		newFunction("XXXX_last", () => Math.random() <= 0.5)
			.call()
			.test.is(1)
			.success(
				getVar("sentence")
					.set(
						v => v
							.replace("XXXX", "ZZZZ")
							.replace("YYYY", "XXXX")
							.replace("ZZZZ", "YYYY")
					)
			)
		,
		
		newText("sentence")
			.before(newText("p", "<p>"))
			.text(getVar("sentence"))
			.after(newText("close_p", "</p>"))
			.center()
			.print()
		,
		
		newText("sep", "___________________________________________")
			.center()
			.print()
		,
		
		newText("question", "<p>Where is <i>" + variable.word + "</i> more likely to go?</p>")
			.center()
			.print()
		,
		
		newSelector("position")
		,
		
		newCanvas("buttons", 200, 50)
			.add(              0, 0, newButton("XXXX").selector("position"))
			.add("right at 100%", 0, newButton("YYYY").selector("position"))
			.center()
			.print()
		,
		
		getSelector("position")
			.shuffle()
			.once()
			.wait()
			.log()
		,
		
		getCanvas("buttons")
			.remove()
		,
		
		newButton("Next")
			.center()
			.print()
			.wait()
	)
	.log("group"		, variable.group)
	.log("item"			, variable.item)
	.log("word"			, variable.word)
	.log("args_group"	, variable.args_group)
	.log("sentence_type", variable.sentence_type)
)

/*
PennController("feedback",
	newText(
		"If you have any feedback on the experiment, please leave it here. " +
		"We would be especially interested to hear if you have any thoughts about words you think might be " +
		"similar in meaning to <i>blork</i>." + "
		"<p />If you don't have any feedback to leave, " +
		"you can leave this blank and continue by pressing \"Send\" below.<p />"
	)
		.css(centered_justified_style)
		.print()
	,

	newTextInput("feedback", "")
		.center()
		.log()
		.lines(0)
		.size(420, 200)
		.print()
	,

	newButton("Send")
		.center()
		.print()
		.wait()
)

newTrial("bye",
	newText(
		"Thank you for your participation! " +
		"Please go to the following web page to verify your participation: " +
		"<a href='about:blank'>(placeholder)</a>.")
		.print()
	,
	
	newButton().wait()	// Wait for a click on a non-displayed button = wait here forever
)
.setOption("countsForProgressBar", false)
*/