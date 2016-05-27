/* 
Feel free to modify and use it as you wish, no restrictions. 
I'd like to hear if it's used anywhere though!

Thanks to Serge for providing a good example of the base mouse tracking here: cdpn.io/rbgpD
*/

var eye, lids = null;
var lidMax = 76;
var skinColor = $('.lids').css('border-top-color');
var eyeColor;
var lidTop = {
    pos: 25,
    posGoal: this.pos,
    relaxed: 25,
    surprised: 10,
    angry: 70,
    playful: 10,
    bored: 55,
    tired: 70,
    squint: 60,
    lerp: 0,
    modifier: 1,
};
var lidBottom = {
    pos: 25,
    posGoal: this.pos,
    relaxed: 25,
    surprised: 10,
    angry: 30,
    playful: 50,
    bored: 15,
    tired: 55,
    squint: 65,
    lerp: 0,
    modifier: 1
};
var iris = { ref: null, x: 0, y: 0, w: 60, h: 60, color: '', lerp: 0 };
var pupil = { ref: null, x: 0, y: 0, size: 30, sizeGoal: 30, lerp: 0 };
var blinkFlag = false;
var blinkTimer = 0;
var distractionTimer = 0;
var wakeTimer = randomInt(240, 720);
var distractedFlag = true;
var mouse = { x: 50, y: 50, oldX: 50, oldY: 50 };
var boredom = 0,
    shock = 0,
    anger = 0;
/* var eyeSounds = document.getElementsByClassName("eye-poke"); */
/* var skinSounds = document.getElementsByClassName("skin-poke"); */

var r = iris.w / 2;
var center = {
    x: $('.eye').width() / 2 - r,
    y: $('.eye').height() / 2 - r
};
var distanceThreshold = $('.eye').width() / 2 - r;
var xp = 45;
var yp = 55;

window.onload = init;

function init() {
    eye = $('.eye');
    eyeColor = eye.css('background-color');
    eyeColor = eyeColor.replace(/[^\d,.]/g, '').split(',');
    eyeColor[0] = parseInt(eyeColor[0]);
    eyeColor[1] = parseInt(eyeColor[1]);
    eyeColor[2] = parseInt(eyeColor[2]);

    lids = $('.lids');

    iris.ref = $('.iris');
    iris.x = parseInt(iris.ref.css('left'));
    iris.y = parseInt(iris.ref.css('top'));
    iris.w = parseInt(iris.ref.css('width'));
    iris.h = parseInt(iris.ref.css('height'));
    iris.color = iris.ref.css('background-color');

    pupil.ref = $('.pupil');
    pupil.x = parseInt(pupil.ref.css('left'));
    pupil.y = parseInt(pupil.ref.css('top'));
    pupil.w = parseInt(pupil.ref.css('width'));
    pupil.h = parseInt(pupil.ref.css('height'));

    animate();
}

function animate() {
    blinkTimer -= 1 + anger / 50;
    if (blinkTimer <= 0) {
        blinkTimer = randomInt(120, 600); /*60 = 1s */
        blinkFlag = true;
    }

    distractionTimer -= 1;

    if (distractionTimer <= 0) {
        distractionTimer = randomInt(60, 120);

        if (distractedFlag === true) {
            var eyeposx = parseInt(eye.css('left'));
            var eyeposy = parseInt(eye.css('top'));
            var tempX = randomInt((mouse.x + eyeposx + r) - 200, (mouse.x + eyeposx + r) + 200);
            var tempY = randomInt((mouse.y + eyeposx + r) - 200, (mouse.y + eyeposx + r) + 100);

            var d = {
                x: tempX - r - eyeposx - center.x,
                y: tempY - r - eyeposy - center.y
            };
            var distance = Math.sqrt(d.x * d.x + d.y * d.y);
            if (distance < distanceThreshold) {
                mouse.x = tempX - eyeposx - r;
                mouse.y = tempY - eyeposy - r;
            } else {
                mouse.x = d.x / distance * distanceThreshold + center.x;
                mouse.y = d.y / distance * distanceThreshold + center.y;
            }
        }
        distractedFlag = true;
    }

    followMouse();
    updateEmotions();
    blink();
    updateEyeParts();
    mouse.oldX = mouse.x;
    mouse.oldY = mouse.y;
    setTimeout(animate, 16);
}

function updateEyeParts() {
    /* ensure lids close/open properly */
    if (lidTop.pos >= lidMax) {
        lidTop.pos = lidMax;
    } else if (lidTop.pos <= 0) {
        lidTop.pos = 0;
    }

    if (lidBottom.pos >= lidMax) {
        lidBottom.pos = lidMax;
    } else if (lidBottom.pos <= 0) {
        lidBottom.pos = 0;
    }

    /* update eye "white" part */
    var rgbString = 'rgb(' + Math.round(eyeColor[0]) + ',' +
        Math.round(eyeColor[1]) + ',' + Math.round(eyeColor[2]) + ')';
    eye.css('background-color', rgbString);

    /* pupil focus */
    pupil.size = interpolate(pupil.size, pupil.sizeGoal, pupil.lerp, 0.03);
    pupil.x = iris.w / 2 - pupil.size / 2;
    pupil.y = iris.h / 2 - pupil.size / 2;

    /* top lid */
    lids.css('border-top', lidTop.pos + 'px solid ' + skinColor);

    /* bottom lid */
    lids.css('border-bottom', lidBottom.pos + 'px solid ' + skinColor);

    /* iris/pupil movement and color */
    iris.ref.css('left', iris.x + 'px');
    iris.ref.css('top', iris.y + 'px');
    iris.ref.css('background', iris.color);

    /* pupil */
    pupil.ref.css('left', pupil.x + 'px');
    pupil.ref.css('top', pupil.y + 'px');
    pupil.ref.css('width', pupil.size + 'px');
    pupil.ref.css('height', pupil.size + 'px');
}

/* note: does a bit too much, needs revising/refactoring. 
  It's supposed to just do the blinking animation but it's
  starting to handle all of the eyelid animations.
*/
function blink() {
    if (blinkFlag) {
        lidTop.pos = interpolate(lidTop.pos, lidMax, lidTop.lerp, 0.6);
        lidBottom.pos = interpolate(lidBottom.pos, lidMax, lidBottom.lerp, 0.6);
    } else if (!blinkFlag) {
        if (anger >= 50) {
            lidTop.goalPos = lidTop.angry / lidTop.modifier;
            lidBottom.goalPos = lidBottom.angry / lidBottom.modifier;
        } else if (shock >= 50) {
            lidTop.goalPos = lidTop.surprised / lidTop.modifier;
            lidBottom.goalPos = lidBottom.surprised / lidBottom.modifier;
        } else if (boredom >= 150) {
            lidTop.goalPos = lidTop.goalPos + 1 / lidTop.modifier;
            lidBottom.goalPos = lidBottom.goalPos + 1 / lidBottom.modifier;
        } else if (boredom >= 50) {
            lidTop.goalPos = lidTop.bored / lidTop.modifier;
            lidBottom.goalPos = lidBottom.bored / lidBottom.modifier;
        } else {
            lidTop.goalPos = lidTop.relaxed / lidTop.modifier;
            lidBottom.goalPos = lidBottom.relaxed / lidBottom.modifier;
        }

        lidTop.pos = interpolate(lidTop.pos, lidTop.goalPos, lidTop.lerp, 0.3);
        lidBottom.pos = interpolate(lidBottom.pos, lidBottom.goalPos, lidBottom.lerp, 0.3);
    }

    if (lidTop.pos >= lidMax - 1 && lidBottom.pos >= lidMax - 1) {
        blinkFlag = false;
    }
}

function updateEmotions() {
    boredom += 0.1;
    if (boredom >= 170) {
        boredom = 170;

        wakeTimer -= 1;
        if (wakeTimer <= 0) {
            wakeTimer = randomInt(180, 720);
            boredom = 0;
        }
    } else if (boredom <= 0) {
        boredom = 0;
    }

    anger -= 0.15;
    if (anger >= 150) {
        anger = 150;
    } else if (anger <= 0) {
        anger = 0;
    }

    eyeColor[0] = interpolate(eyeColor[0], 245 + anger, 0, 0.008);
    eyeColor[1] = interpolate(eyeColor[1], 240 - anger, 0, 0.008);
    eyeColor[2] = interpolate(eyeColor[2], 240 - anger, 0, 0.008);
    eyeColor.forEach(function(element, index) {
        if (eyeColor[index] >= 255) {
            eyeColor[index] = 255;
        } else if (eyeColor[index] <= 0) {
            eyeColor[index] = 0;
        }
    });
}

/* mouse movement event function */
$(window).on('mousemove', function(e) {
    distractedFlag = false;

    var eyeposx = parseInt($('.eye').css('left'));
    var eyeposy = parseInt($('.eye').css('top'));

    var d = {
        x: e.pageX - r - eyeposx - center.x,
        y: e.pageY - r - eyeposy - center.y
    };
    var distance = Math.sqrt(d.x * d.x + d.y * d.y);
    if (distance < distanceThreshold) {
        mouse.x = e.pageX - eyeposx - r;
        mouse.y = e.pageY - eyeposy - r;
    } else {
        mouse.x = d.x / distance * distanceThreshold + center.x;
        mouse.y = d.y / distance * distanceThreshold + center.y;
    }

    /* make eyelids close more the closer the mouse gets to them */
    var lidModifier = (distance / 50);
    if (distance > 50) {
        lidModifier = 1;
    } else {
        /* decrease blink interval if moving over eye */
        blinkTimer -= 3;
    }
    if (lidModifier < 0.8) {
        lidModifier = 0.8;
    }

    lidTop.modifier = lidModifier;
    lidBottom.modifier = lidModifier;

    /* pupil "focuses" the closer the mouse is */
    if (distance < 100) {
        pupil.sizeGoal = lidModifier * 30;
    } else {
        pupil.sizeGoal = 30;
    }

    /* decrease boredom because of mouse movement */
    boredom -= 1;
});

/* move eye according to mouse movement */
function followMouse() {
    var lerpSpeed = 0.12;
    xp = interpolate(xp, mouse.x, 0, lerpSpeed);
    yp = interpolate(yp, mouse.y, 0, lerpSpeed);

    var distance = Math.sqrt((mouse.x - mouse.oldX) * (mouse.x - mouse.oldX) +
        (mouse.y - mouse.oldY) * (mouse.y - mouse.oldY));

    /* simulate saccade eye movement */
    if (distance >= 25) {
        xp = interpolate(xp, mouse.x, 0, 0.4);
        yp = interpolate(yp, mouse.y, 0, 0.4);
    }

    iris.x = xp;
    iris.y = yp;
}

$('.lids').on('click tap touchmove touchstart swipe touchend', function() {
    anger += 100;
    blinkTimer = 1;
    
});

$('*').on('click tap touchmove touchstart swipe touchend', function() {
    anger += 100;
    blinkTimer = 1;
});

/* linearly interpolate from part to goalPos (smooth animation effect) */
function interpolate(part, goalPos, currentLerp, lerpSpeed) {
    if (part != goalPos) {
        currentLerp = 0;
    }

    if (currentLerp <= 1.0) {
        currentLerp += lerpSpeed;
    }

    part = lerp(part, currentLerp, goalPos);

    return part;
}

/* actual formula for linear interpolation  */
function lerp(x, t, y) {
    return x * (1 - t) + y * t;
}

/* get random integer in range min-max */
function randomInt(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}
