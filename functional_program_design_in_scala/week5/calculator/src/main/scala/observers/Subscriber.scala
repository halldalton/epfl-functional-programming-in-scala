package observers

trait Subscriber {

    def handler(pub: Publisher)

}
