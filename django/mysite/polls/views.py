from django.http import HttpResponse, Http404
from django.shortcuts import get_object_or_404, render

from . import models


# Create your views here.
def index(request):
    latest_questions = models.Question.objects.order_by('-pub_date')[:5]
    ctx = {'latest_questions': latest_questions}
    return render(request, 'polls/index.html', ctx)

def detail(request, question_id):
    q = get_object_or_404(models.Question, pk=question_id)
    return render(request, 'polls/detail.html', {'question': q})

def results(request, question_id):
    response = "Tally for \"{}\":".format(question_id)
    return HttpResponse(response)

def vote(request, question_id):
    return HttpResponse("You're voting on {}".format(question_id))

