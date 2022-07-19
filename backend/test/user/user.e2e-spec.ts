const runUserTests = (pactum: any) => {
    describe('User', () => {
        it(`/users/me (GET)`, () => {
          return pactum
          .spec()
          .get(`/users/me`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectBodyContains("intraID")
        });
    
        it(`/users/me (PATCH)`, () => {
          const dto = {
            email : "newemail@mail.com"
          }
          return pactum
          .spec()
          .patch(`/users/me`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectBodyContains(dto.email)
        });
    
        it(`/users/me (PATCH)`, () => {
          const dto = {
            email : "newemail2@mail.com",
            level : 0.1
          }
          return pactum
          .spec()
          .patch(`/users/me`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectBodyContains(dto.email)
          .expectBodyContains(dto.level)
        });
    
        it(`/users/me (PATCH)`, () => {
          const dto = {
            intraID : "new"
          }
          return pactum
          .spec()
          .patch(`/users/me`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectBodyContains("jng")
        });
    
        it(`/users/me (PATCH)`, () => {
          const dto = {
            username : "new"
          }
          return pactum
          .spec()
          .patch(`/users/me`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectBodyContains(dto.username)
        });
    
        it(`/users/me (PATCH)`, () => {
          const dto = {
            username : "tes"
          }
          return pactum
          .spec()
          .patch(`/users/me`)
          .withBody(dto)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(400)
        });
    
        it(`/users (GET) (Pagination)`, () => {
          return pactum
          .spec()
          .get(`/users?page=1&pageSize=1`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonLength("data", 1)
        });
    
        it(`/users (GET) (Pagination)`, () => {
          return pactum
          .spec()
          .get(`/users?page=2&pageSize=1`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonLength("data", 1)
        });
        
        it(`/users (GET) (Pagination)`, () => {
          return pactum
          .spec()
          .get(`/users?page=3&pageSize=1`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonLength("data", 1)
        });
    
        it(`/users (GET) (Pagination)`, () => {
          return pactum
          .spec()
          .get(`/users?page=1000&pageSize=1`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonLength("data", 0)
        });
    
        it(`/users (GET) (Pagination)`, () => {
          return pactum
          .spec()
          .get(`/users?page=a&pageSize=1`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(400)
        });
    
        it(`/users (GET) (Pagination)`, () => {
          return pactum
          .spec()
          .get(`/users?page=1&pageSize=a`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(400)
        });
    
        it(`/users (GET) (Filtering)`, () => {
          return pactum
          .spec()
          .get(`/users?page=1&pageSize=3&filterOn=status&filterBy=OFFLINE`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonLength("data", 3)
        });
    
        it(`/users (GET) (Filtering)`, () => {
          return pactum
          .spec()
          .get(`/users?page=1&pageSize=3&filterOn=status,level&filterBy=OFFLINE,1.12`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonLength("data", 3)
        });
    
        it(`/users (GET) (sorting)`, () => {
          return pactum
          .spec()
          .get(`/users?page=1&pageSize=3&sortBy=Ascending&sortOn=level`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonMatch("data[0]", {level : 0.1})
        });
    
        it(`/users (GET) (sorting)`, () => {
          return pactum
          .spec()
          .get(`/users?page=1&pageSize=3&sortBy=Descending&sortOn=level`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonMatch("data[0]", {level : 1.12})
        });
    
        it(`/users (GET) (sorting)`, () => {
          return pactum
          .spec()
          .get(`/users?page=1&pageSize=3&sortBy=Ascending&sortOn=username`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(200)
          .expectJsonMatch("data[0]", {username : "new"})
        });
    
        it(`/users (GET) (sorting)`, () => {
          return pactum
          .spec()
          .get(`/users?page=1&pageSize=3&sortBy=Descendng&sortOn=level`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(400)
        });
    
        it(`/users (GET) (sorting)`, () => {
          return pactum
          .spec()
          .get(`/users?page=1&pageSize=3&sortBy=Ascending&sortOn=lvel`)
          .withHeaders({
            Authorization : 'Bearer $S{token}',
          })
          .expectStatus(400)
        });
    
      })
}

export default runUserTests;