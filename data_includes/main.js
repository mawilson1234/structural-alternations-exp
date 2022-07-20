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

// var counterOverride = 0

Sequence(
	"setcounter", 
	// "intro", "consent", "recording", "instruction", 
	randomize("trial_prac"), 
	// "warn", "instruction2", 
	//rshuffle("pretrial"), 
	//rshuffle("trial"), 
	// "feedback", 
	SendResults(), 
	"bye"
)

/*newTrial( "intro",
	newText("Welcome","Welcome! This experiment has two halves. Following the first half, you will see a link to the second half, which will have different instructions you will see at that time.<p>To participate in this experiment, you must meet the following requirements.<p>(1) Your computer must have a microphone (a built-in microphone is fine).<p>(2) Your browser must be either Chrome or Firefox. You CANNOT use Safari for this experiment.<p>(3) You must turn off music/video (e.g., YouTube) played on the same computer you are using to take this experiment.<p>(4) Please note that you will be asked to speak aloud during the experiment (recite simple sentences and pronounce fake words aloud). Your speech will be recorded and that's our critical data.<p>If you meet these requirements, please enter your Prolific ID below and click Next:")
		.settings.css("font-size", "2em")
		.print()
	,
	
	newTextInput("ProlificID")
		.before(
			newText("ID", "Your Prolific ID:")
				.settings.css("font-size", "2em")
		)
		.settings.css("font-size", "2em")
		.settings.css('width', '50%')
		.settings.css('margin', 'auto')
		.print()
		.log()
	,
	
	newButton("Next","Next")
		.center()
		.settings.css("font-size", "2em")
		.settings.css('margin', '40px')
		.settings.size(500, 48)
		.print()
		.wait()
)

newTrial( "consent" ,
	newText("Please click <a href='https://shotam.github.io/IRB/consent_online_recording.pdf' target='_blank'>here</a> to download the consent form for this study. If you read it and agree to participate in this study, click 'I Agree' below. If you do not agree to participate in this study, you can leave this study by closing the tab.")
		.settings.css("font-size", "2em")
		.print()
	,
	
	newButton("Agree","I Agree")
		.center()
		.settings.css("font-size", "2em")
		.settings.css('margin', '40px')
		.settings.size(500, 48)
		.print()
		.wait()
)

newTrial("instruction",
	newText("Instr", "In this experiment, you will first read a sentence in a word-by-word fashion, then pronounce a series of fake words out loud, and then say out loud the sentence you memorized.<p>When reading the sentence, you will first see a long dash appear. Press the Space bar to show the first word when you are ready. When you are finished reading each word, you should press the Space bar to proceed, which will replace the previous word with the next one. Your task is to read the sentence silently and <b>memorize the sentence for later recall.</b></p> <p>After you see each sentence, you will see five stars, like this '*****'. When you are ready, you should press the Space bar again to proceed to the second task, which is to <b>pronounce each fake word out loud</b> as it is presented to you <b>as soon as possible</b>. If you are unsure about how to pronounce a particular fake word, just try your best. After you have pronounced the fake words, you will see the text 'Recall Sentence' appear on the screen. When you see 'Recall Sentence' appear, you should recite the sentence you memorized earlier <b>aloud</b>, as soon as possible.</p> <p><b>TIP: Many people find it helpful to try to visualize the situations described by sentences when memorizing them.</b></p>")
		.settings.css("font-size", "2em")
		.print()
	,

	newButton("Click","Click here to begin practice trials!")
		.center()
		.settings.css("font-size", "2em")
		.settings.css("margin", "40px")
		.settings.size(500, 48)
		.print()
		.wait()
)
*/

/*Template("practice.csv", variable => 
	newTrial("trial_prac",
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
	.log("item"			, variable.item)
	.log("word"			, variable.word)
	.log("args_group"	, variable.args_group)
	.log("sentence_type", variable.sentence_type)
)*/

Template("practice.csv", variable => {
	var word = variable['word_' + Math.floor(Math.random() * 12)];
	return newTrial("trial_prac",		
		newText("container", "")
			.center()
			.css("display", "flex")
			.css("margin-bottom", "3em")
			.print()
		,
		// get the first part of the sentence (before XXXX)
		newText(variable.sentence.match(/^(.*?)(?=\[(su|o)bj\])/g)[0] + '&nbsp;')
			.print(getText("container"))
		,
		
		newText(variable.sentence.match(/\[(su|o)bj\]/g)[0], " ")
			.css(blank_style)
			.print(getText("container"))
		,
		// get the middle part of the sentence (between XXXX and YYYY)
		newText('&nbsp;' + variable.sentence.match(/(?<=\[(su|o)bj\]).*?(?=\[(su|o)bj\])/g)[0] + '&nbsp;')
			.print(getText("container"))
		,
		
		newText(variable.sentence.match(/\[(su|o)bj\]/g)[1], " ")
			.css(blank_style)
			.print(getText("container"))
		,
		// get the final part of the sentence (after YYYY)
		newText('&nbsp;' + variable.sentence.match(/.*(?<=\[(su|o)bj\])(.*?)$/)[2])
			.print(getText("container"))
		,
		
		newText("placeholder", "&mdash;")
			.center()
			.print()
		,
		
		newTimer("wait", variable.sentence.split(" ").length * 250)
			.start()
			.wait()
		,
		
		getText("placeholder")
			.remove()
		,
		
		newText("word", word)
			.css({
				border: '1px solid #000',
				padding: '3px'
			})
			.center()
			.print()
		,
		
		newMouseTracker("mouse")
			.log()
			.start()
		,
		
		newDragDrop("dd", "bungee")
			.log("all")
			.addDrop(
				getText(variable.sentence.match(/(?<=\[)(su|o)bj(?=\])/g)[0]), 
				getText(variable.sentence.match(/(?<=\[)(su|o)bj(?=\])/g)[1])
			)
			.addDrag(getText("word"))
			.offset('0.5em', '0.1em', 
				getText(variable.sentence.match(/(?<=\[)(su|o)bj(?=\])/g)[0]), 
				getText(variable.sentence.match(/(?<=\[)(su|o)bj(?=\])/g)[1])
			)
			.wait()
			.removeDrag(getText("word"))
			.removeDrop(
				getText(variable.sentence.match(/(?<=\[)(su|o)bj(?=\])/g)[0]), 
				getText(variable.sentence.match(/(?<=\[)(su|o)bj(?=\])/g)[1])
			)
		,
		
		getMouseTracker("mouse")
			.stop()
		,
		
		getText("word")
			.css({border: '', width: '', 'padding-top': ''})
		,
		
		newButton("next", "Next")
			.css("margin-top", "2em")
			.center()
			.print()
			.wait()
			.remove()
	)
	.log('item'		 	, variable.item)
	.log('word'			, word)
	.log('args_group'	, variable.args_group)
	.log('sentence_type', variable.sentence_type)
	.log('sentence'	 	, variable.sentence);
})

/*
newTrial("warn",
	newText("Practice done!<p><b>Please note: some participants have reported that the script froze in the middle of the experiment. If this happens to you, please don’t panic, and let us know via the message function in Prolific. We will make sure that you will be compensated for the time you spent for the experiment.</b></p>")
		.settings.css("font-size", "2em")
		.print()
	,
	newButton("Next", "Next")
		.center()
		.settings.css("font-size", "2em")
		.settings.css('margin', '40px')
		.settings.size(500, 48)
		.print()
		.wait()
)

newTrial("instruction2",
	newText("Instr2", "Now, you are ready to start the experiment! Remember, your task is to:<p>(1) Silently read and memorize sentences presented in a word-by-word fashion.<p>(2) Read aloud each fake word presented after the sentence.<p>(3) When you see the words 'Recall Sentence,' say the sentence you memorized out loud.")
		.settings.css("font-size", "2em")
		.print()
	,

	newButton("Click","Click here to begin the experiment")
		.center()
		.settings.css("font-size", "2em")
		.settings.css('margin', '40px')
		.settings.size(550, 48)
		.print()
		.wait()
)

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

PennController("feedback",
	newText("feedback_instruction",
		"If you have any feedback on the experiment, please leave it here.<p />")
		.center()
		.print()
	,

	newTextInput("feedback", "")
		.center()
		.log()
		.lines(0)
		.size(420, 200)
		.print()
	,

	newButton("send", "Send")
		.center()
		.print()
		.wait()
)
*/

newTrial("bye" ,
	newText("Thank you for your participation!")//" Please go to the following web page to verify your participation: <a href='https://app.prolific.co/submissions/complete?cc=728AA2CF'> https://app.prolific.co/submissions/complete?cc=728AA2CF</a>.")
		.print(),
	
	newButton()
		.wait()	// Wait for a click on a non-displayed button = wait here forever
)
.setOption("countsForProgressBar" , false)
// Make sure the progress bar is full upon reaching this last (non-)trial