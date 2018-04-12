package org.github.meshelton.github_dashboard.controllers

import javax.inject._

import org.github.meshelton.github_dashboard.shared.SharedMessages
import play.api.mvc._

@Singleton
class Application @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def index = Action {
    Ok(views.html.index())
  }

}
