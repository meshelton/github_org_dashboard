package org.github.meshelton.github_dashboard

import org.scalajs.dom.{Node, document}
import org.scalajs.dom.raw.{Event, HTMLInputElement}
import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{Var, Vars}
import cats.effect.IO
import github4s.Github
import github4s.Github._
import github4s.cats.effect.js.Implicits._
import fr.hmil.roshttp.response.SimpleHttpResponse
import github4s.free.domain.{Repository, User}
import org.scalajs.{dom => jsdom}

case class RepoStats(repo: Repository, forks: Int, stars: Int, contributors: List[User])

object ScalaJSExample {

  val accessToken = "c1cd1a05c09c9b8a31530ee69a281fafb4c2cab7"

  def main(args: Array[String]): Unit = {
    val orgInput: Var[String] = Var("")
    val repos: Var[Option[List[Repository]]] = Var(None)
    val repoStats: Vars[RepoStats] = Vars()
    val  orgMembers: Var[List[User]] = Var(Nil)
    dom.render(document.getElementById("scalajsEntryPoint"), renderMainContainer(orgInput, repos, repoStats, orgMembers))
  }

  @dom def renderMainContainer(orgInput: Var[String], repos: Var[Option[List[Repository]]], reposStats: Vars[RepoStats],  orgMembers: Var[List[User]]): Binding[Node] = {
    <div>
      { renderOrgInput(orgInput).bind }
      { renderOrgDetails(orgInput, repos).bind }
      { renderPopularRepositoryTable(repos, reposStats).bind }
      { renderTopContributors(orgInput, reposStats, orgMembers).bind }
    </div>
  }

  @dom def renderOrgInput(orgInput: Var[String]): Binding[HTMLInputElement] = {
      <input type="text" id="githubOrg"
             value={ orgInput.bind.toString }
             onchange={ (e: Event) => orgInput.value = githubOrg.value }
      />
  }

  @dom def renderOrgDetails(org: Var[String], repos: Var[Option[List[Repository]]]): Binding[Node] = {
    Github(Some(accessToken)).repos.listOrgRepos(org.bind).exec[IO, SimpleHttpResponse]().unsafeRunAsync {
      case Right(s) => repos.value = s match {
        case Right(response) => Some(response.result)
        case Left(error) => None
      }
      case Left(e)  => {
        jsdom.console.log("Something went wrong")
      }
    }
    <div>
      {
       repos.bind match {
         case Some(repos) => s"${org.bind} has ${repos.length} repositories"
         case None => s"${org.bind} is not a valid github organization"
       }
      }
    </div>
  }

  @dom def renderTopContributors(org: Var[String], reposStats: Vars[RepoStats], orgMembers: Var[List[User]]): Binding[Node] = {
    Github(Some(accessToken)).organizations.listMembers(org.bind).exec[IO, SimpleHttpResponse]().unsafeRunAsync {
      case Right(s) => orgMembers.value = s match {
        case Right(response) => response.result
        case Left(error) => Nil
      }
      case Left(e) => jsdom.console.log("Something went wrong")
    }
    val orgUsers = orgMembers.bind.toSet
    val (inOrgContributors, outOrgContributors) = reposStats.bind.flatMap(_.contributors).groupBy(x => x).mapValues(_.length).partition{ case (user, _) => orgUsers.contains(user) }
    val sortedIn = inOrgContributors.toList.sortBy(_._2).map(_._1)
    val sortedOut = outOrgContributors.toList.sortBy(_._2).map(_._1)
    <div>
      <div>Top Contributors In Org</div>
      <ol>
        {
        import scalaz.std.list._
        for (user <- sortedIn) yield {
          <li>{user.login}</li>
        }
        }
      </ol>
      <div>Top Contributors outside Org</div>
      <ol>
        {
        import scalaz.std.list._
        for (user <- sortedOut) yield {
          <li>{user.login}</li>
        }
        }
      </ol>
    </div>
  }

  @dom def renderPopularRepositoryTable(repos: Var[Option[List[Repository]]], reposStats: Vars[RepoStats]): Binding[Node] = {
    val newRepos = repos.bind.getOrElse(Nil)
      reposStats.value.clear()
      newRepos.foreach( repo => {
      Github(Some(accessToken)).repos.listContributors(repo.owner.login, repo.name).exec[IO, SimpleHttpResponse]().unsafeRunAsync {
        case Right(s) => s match {
          case Right(response) => reposStats.value += RepoStats(repo, repo.status.forks_count, repo.status.stargazers_count, response.result)
          case Left(error) => reposStats.value += RepoStats(repo, repo.status.forks_count, repo.status.stargazers_count, Nil)
        }
        case Left(e)  => reposStats.value += RepoStats(repo, repo.status.forks_count, repo.status.stargazers_count, Nil)
      }
    })
    val repoList = reposStats.bind
    val byForks = repoList.sortBy(_.forks).reverse
    val byLikes = repoList.sortBy(_.stars).reverse
    val byContrbutors = repoList.sortBy(_.contributors.length).reverse
    val popularLists = byForks.zip(byLikes).zip(byContrbutors).map{ case ((fork, like), contribute) => (fork, like, contribute)}.toList


    <table>
      <tr><td>Most Forked</td><td>Most starred</td>Most Contributers</tr>
      {
      import scalaz.std.list._
      for ((fork, like, contribute) <- popularLists) yield {
          <tr><td>{fork.repo.name}</td><td>{like.repo.name}</td><td>{contribute.repo.name}</td></tr>
        }
      }
    </table>
  }

}