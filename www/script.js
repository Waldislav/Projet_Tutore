document.addEventListener("DOMContentLoaded", function() {
    Shiny.bindAll();
});

document.querySelectorAll('.nav-link').forEach(anchor => {
    anchor.addEventListener('click', function(e) {
        e.preventDefault();
        const target = document.querySelector(this.getAttribute('href'));
        const offset = 70;
        const bodyRect = document.body.getBoundingClientRect().top;
        const elementRect = target.getBoundingClientRect().top;
        const elementPosition = elementRect - bodyRect;
        const offsetPosition = elementPosition - offset;
        console.log(offsetPosition);
        window.scrollTo({
            top: offsetPosition,
            behavior: 'smooth'
        });
    });
});

function scroll_intro () {
    const target = document.getElementById('a-propos');
    const offset = 70;
    const bodyRect = document.body.getBoundingClientRect().top;
    const elementRect = target.getBoundingClientRect().top;
    const elementPosition = elementRect - bodyRect;
    const offsetPosition = elementPosition - offset;
    console.log(offsetPosition);
    document.getElementById('a-propos').scrollIntoView({top: offsetPosition, behavior: 'smooth' });
};

Shiny.addCustomMessageHandler('openAccordion', function(message) {
    let acc = document.getElementById(message.id);
    if (acc && acc.getElementsByClassName('accordion-panel')[0]) {
      acc.getElementsByClassName('accordion-panel')[0].click();
    }
  });